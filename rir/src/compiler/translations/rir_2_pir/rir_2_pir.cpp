#include "rir_2_pir.h"
#include "../../analysis/query.h"
#include "../../analysis/verifier.h"
#include "../../pir/pir_impl.h"
#include "../../transform/insert_cast.h"
#include "../../util/builder.h"
#include "../../util/cfg.h"
#include "../../util/visitor.h"
#include "R/RList.h"
#include "ir/BC.h"

#include <deque>
#include <vector>

namespace {

using namespace rir::pir;
typedef rir::Function Function;
typedef rir::Opcode Opcode;
typedef rir::BC BC;
typedef rir::RList RList;

typedef std::pair<BB*, Value*> ReturnSite;

template <size_t SIZE>
struct Matcher {
    const std::array<Opcode, SIZE> seq;

    typedef std::function<void(Opcode*)> MatcherMaybe;

    bool operator()(Opcode* pc, Opcode* end, MatcherMaybe m) const {
        for (size_t i = 0; i < SIZE; ++i) {
            if (*pc != seq[i])
                return false;
            BC::advance(&pc);
            if (pc == end)
                return false;
        }
        m(pc);
        return true;
    }
};

void recoverCFG(rir::Code* srcCode, rir::Function* srcFunction,
                std::unordered_map<Opcode*, StackMachine>& mergepoint) {
    std::unordered_map<Opcode*, std::vector<Opcode*>> incom;
    // Mark incoming jmps
    for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
        BC bc = BC::decode(pc);
        if (bc.isJmp()) {
            incom[bc.jmpTarget(pc)].push_back(pc);
        }
        BC::advance(&pc);
    }
    // Mark falltrough to label
    for (auto pc = srcCode->code(); pc != srcCode->endCode();) {
        BC bc = BC::decode(pc);
        if (!bc.isUncondJmp() && !bc.isReturn()) {
            Opcode* next = BC::next(pc);
            if (incom.count(next))
                incom[next].push_back(pc);
        }
        BC::advance(&pc);
    }
    // Create mergepoints
    for (auto m : incom)
        if (std::get<1>(m).size() > 1)
            mergepoint.emplace(m.first,
                               StackMachine(srcFunction, srcCode, m.first));
}

} // namespace

namespace rir {
namespace pir {

Value* Rir2Pir::translate(rir::Code* srcCode, Builder& insert) const {
    assert(!finalized);

    std::unordered_map<Opcode*, StackMachine> mergepoint;
    std::vector<ReturnSite> results;

    recoverCFG(srcCode, srcFunction, mergepoint);

    std::deque<StackMachine> worklist;

    StackMachine state(srcFunction, srcCode);

    auto popFromWorklist = [&]() {
        assert(!worklist.empty());
        state = worklist.back();
        worklist.pop_back();
        insert.bb = state.getEntry();
    };

    while (!state.atEnd() || !worklist.empty()) {
        if (state.atEnd())
            popFromWorklist();

        BC bc = state.getCurrentBC();

        if (mergepoint.count(state.getPC()) > 0) {
            StackMachine* other = &mergepoint.at(state.getPC());
            bool todo = state.doMerge(state.getPC(), insert, other);
            state = mergepoint.at(state.getPC());
            insert.next(state.getEntry());
            if (!todo) {
                if (worklist.empty()) {
                    state.clear();
                    break;
                }
                popFromWorklist();
                continue;
            }
        }

        if (bc.isJmp()) {
            auto trg = bc.jmpTarget(state.getPC());
            Opcode* fallpc = BC::next(state.getPC());
            if (bc.isUncondJmp()) {
                state.setPC(trg);
                continue;
            }

            // Conditional jump
            switch (bc.bc) {
            case Opcode::brtrue_:
            case Opcode::brfalse_: {
                Value* v = state.pop();
                insert(new Branch(v));
                break;
            }
            case Opcode::brobj_: {
                Value* v = insert(new IsObject(state.top()));
                insert(new Branch(v));
                break;
            }
            default:
                assert(false);
            }

            BB* branch = insert.createBB();
            BB* fall = insert.createBB();

            // TOS == TRUE goes to next1, TOS == FALSE goes to next0
            switch (bc.bc) {
            case Opcode::brtrue_:
                insert.bb->next0 = fall;
                insert.bb->next1 = branch;
                break;
            case Opcode::brfalse_:
            case Opcode::brobj_:
                insert.bb->next0 = branch;
                insert.bb->next1 = fall;
                break;
            default:
                assert(false);
            }

            switch (bc.bc) {
            case Opcode::brtrue_:
            case Opcode::brfalse_: {
                state.setPC(trg);
                state.setEntry(branch);
                worklist.push_back(state);
                break;
            }
            case Opcode::brobj_: {
                state.setPC(trg);
                state.setEntry(branch);
                insert.bb = branch;
                Value* front = state.front();
                insert(new Deopt(insert.env, state.getPC(), state.stack_size(),
                                 &front));
                break;
            }
            default:
                assert(false);
            }

            state.setPC(fallpc);
            state.setEntry(fall);
            insert.bb = fall;
            continue;
        }

        if (bc.isReturn()) {
            switch (bc.bc) {
            case Opcode::ret_:
                break;
            case Opcode::return_:
                std::cerr << "Cannot compile Function. Unsupported bc\n";
                bc.print();
            default:
                assert(false);
            }
            results.push_back(ReturnSite(insert.bb, state.pop()));
            assert(state.empty());
            if (worklist.empty()) {
                state.clear();
                break;
            }
            popFromWorklist();
            continue;
        }

        const static Matcher<4> ifFunctionLiteral(
            {{{Opcode::push_, Opcode::push_, Opcode::push_, Opcode::close_}}});

        bool matched = false;

        ifFunctionLiteral(state.getPC(), srcCode->endCode(), [&](Opcode* next) {
            Opcode* pc = state.getPC();
            BC ldfmls = BC::advance(&pc);
            BC ldcode = BC::advance(&pc);
            BC ldsrc = BC::advance(&pc);
            BC::advance(&pc); // close

            SEXP fmls = ldfmls.immediateConst();
            SEXP code = ldcode.immediateConst();
            SEXP src = ldsrc.immediateConst();
            auto fmlsList = RList(fmls);

            std::vector<SEXP> fmlsNames;
            for (auto it = fmlsList.begin(); it != fmlsList.end(); ++it) {
                fmlsNames.push_back(it.tag());
            }

            DispatchTable* dt = DispatchTable::unpack(code);
            rir::Function* function = dt->first();

            Closure* innerF = compiler.compileFunction(function, fmlsNames);

            state.push(
                insert(new MkFunCls(innerF, insert.env, fmls, code, src)));

            matched = true;
            state.setPC(next);
        });

        if (!matched) {
            int size = state.stack_size();
            state.runCurrentBC(*this, insert);
            assert(state.stack_size() == size - bc.popCount() + bc.pushCount());
            state.advancePC();
        }
    }
    assert(state.empty());

    Value* res;
    assert(results.size() > 0);
    if (results.size() == 1) {
        insert.bb = results.back().first;
        res = results.back().second;
    } else {
        BB* merge = insert.createBB();
        insert.bb = merge;
        Phi* phi = insert(new Phi());
        for (auto r : results) {
            r.first->next0 = merge;
            phi->addInput(r.first, r.second);
        }
        phi->updateType();
        res = phi;
    }

    results.clear();

    return res;
}

void Rir2Pir::finalize(Value* ret, Builder& insert) {
    assert(!finalized);
    assert(ret);
    assert(!insert.bb->next0 && !insert.bb->next1 &&
           "Builder needs to be on an exit-block to insert return");

    // Remove excessive Phis
    Visitor::run(insert.code->entry, [&](BB* bb) {
        auto it = bb->begin();
        while (it != bb->end()) {
            Phi* p = Phi::Cast(*it);
            if (!p) {
                it++;
                continue;
            }
            if (p->nargs() == 1) {
                if (p == ret)
                    ret = p->arg(0).val();
                p->replaceUsesWith(p->arg(0).val());
                it = bb->remove(it);
                continue;
            }
            p->updateType();
            it++;
        }
    });

    insert(new Return(ret));

    InsertCast c(insert.code->entry);
    c();

    finalized = true;
}

} // namespace pir
} // namespace rir
