#include "scope_resolution.h"
#include "../analysis/query.h"
#include "../analysis/scope.h"
#include "../pir/pir_impl.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"

#include <algorithm>
#include <unordered_map>

namespace {

using namespace rir::pir;
class TheScopeResolution {
  public:
    Closure* function;
    CFG cfg;
    TheScopeResolution(Closure* function)
        : function(function), cfg(function->entry) {}
    void operator()() {
        ScopeAnalysis analysis(function);

        Visitor::run(function->entry, [&](BB* bb) {
            auto ip = bb->begin();
            while (ip != bb->end()) {
                Instruction* i = *ip;
                auto next = ip + 1;
                LdArg* lda = LdArg::Cast(i);
                LdFun* ldf = LdFun::Cast(i);
                Instruction* ld = LdVar::Cast(i);
                StVar* s = StVar::Cast(i);
                StVarSuper* ss = StVarSuper::Cast(i);
                LdVarSuper* sld = LdVarSuper::Cast(i);
                if (lda)
                    ld = lda;
                else if (ldf)
                    ld = ldf;

                if (sld) {
                    // LdVarSuper where the parent environment is known and
                    // local, can be replaced by a simple LdVar
                    auto e = Env::parentEnv(sld->env());
                    if (e) {
                        auto r = new LdVar(sld->varName, e);
                        bb->replace(ip, r);
                    }
                } else if (ss) {
                    // StVarSuper where the parent environment is known and
                    // local, can be replaced by simple StVar, if the variable
                    // exists in the super env.
                    auto e = Env::parentEnv(ss->env());
                    auto aload = analysis.loads.at(ss);
                    if (e && aload.env != AbstractREnvironment::UnknownParent) {
                        if ((Env::isPirEnv(aload.env) &&
                             !aload.result.isUnknown()) ||
                            Env::isStaticEnv(aload.env)) {
                            auto r = new StVar(ss->varName, ss->arg<0>().val(),
                                               aload.env);
                            bb->replace(ip, r);
                        }
                    }
                } else if (s) {
                    // Dead store to non-escaping environment can be removed
                    if (Env::isPirEnv(s->env()) &&
                        !analysis.finalState[s->env()].leaked &&
                        analysis.deadStore(s)) {
                        next = bb->remove(ip);
                    }
                } else if (ld && analysis.loads.count(ld)) {
                    // If we have a non-ambiguous load, we can replace the load
                    // with the actual values.
                    auto aload = analysis.loads.at(ld);
                    auto aval = aload.result;
                    bool localVals = true;
                    if (aval.isUnknown() &&
                        aload.env != AbstractREnvironment::UnknownParent) {
                        // We have no clue what we load, but we know from where
                        ld->env(aload.env);
                    } else {
                        aval.eachSource([&](ValOrig& src) {
                            // inter-procedural scope analysis can drag in
                            // values
                            // from other functions, which we cannot use here!
                            if (src.origin->bb()->owner != function) {
                                localVals = false;
                            }
                        });
                        if (localVals) {
                            aval.ifSingleValue([&](Value* val) {
                                if (ldf) {
                                    auto f = val;
                                    if (!PirType(RType::closure)
                                             .isSuper(f->type)) {
                                        auto fz = new Force(val);
                                        auto ch = new ChkClosure(fz);
                                        bb->replace(ip, fz);
                                        next = bb->insert(ip + 1, ch);
                                        next++;
                                        f = ch;
                                    } else {
                                        next = bb->remove(ip);
                                    }
                                    ld->replaceUsesWith(f);
                                } else {
                                    ld->replaceUsesWith(val);
                                    next = bb->remove(ip);
                                }
                            });
                            if (!aval.isSingleValue() && !aval.isUnknown() &&
                                !ldf) {
                                auto hasCommonPred = [&](BB* load) {
                                    bool success = true;
                                    aval.eachSource([&](ValOrig& src) {
                                        if (!cfg.transitivePredecessors
                                                 [load->id]
                                                     .count(src.origin->bb()))
                                            success = false;
                                    });
                                    return success;
                                };
                                BB* phiPlacement = bb;
                                // Shift phi up until we see at least two inputs
                                // comming from different paths.
                                for (;;) {
                                    bool commonPred = true;
                                    auto preds =
                                        cfg.predecessors[phiPlacement->id];
                                    for (auto pre : preds) {
                                        commonPred =
                                            commonPred && hasCommonPred(pre);
                                    }
                                    if (!commonPred)
                                        break;
                                    phiPlacement = *preds.begin();
                                }
                                auto phi = new Phi;
                                aval.eachSource([&](ValOrig& src) {
                                    phi->addInput(src.origin->bb(), src.val);
                                });
                                phi->updateType();
                                ld->replaceUsesWith(phi);
                                if (phiPlacement == bb)
                                    bb->replace(ip, phi);
                                else
                                    phiPlacement->insert(phiPlacement->begin(),
                                                         phi);
                            }
                        }
                    }
                }
                ip = next;
            }
        });
    }
};
}

namespace rir {
namespace pir {

void ScopeResolution::apply(Closure* function) {
    TheScopeResolution s(function);
    s();
}
}
}
