#ifndef PIR_ENV_ESCAPE_ANALYSIS_H
#define PIR_ENV_ESCAPE_ANALYSIS_H

#include "../analysis/generic_static_analysis.h"

#include <unordered_map>
#include <unordered_set>

namespace rir {
namespace pir {

class EnvSet {
  public:
    typedef std::unordered_set<Value*> Envs;
    Envs envs;

    AbstractResult merge(const EnvSet& other) {
        AbstractResult res;

        for (const auto& f : other.envs) {
            if (!envs.count(f)) {
                envs.insert(f);
                res.update();
            }
        }

        return res;
    }

    void print(std::ostream& out, bool tty) const {
        out << "* envs: ";
        for (auto& a : envs) {
            a->printRef(out);
            out << " ";
        }
        out << "\n";
    }

    Envs::iterator begin() { return envs.begin(); }
    Envs::iterator end() { return envs.end(); }
};

class DeadStoreAnalysis {
    class EnvLeakAnalysis : public StaticAnalysis<EnvSet> {
      public:
        EnvLeakAnalysis(ClosureVersion* cls, LogStream& log)
            : StaticAnalysis("envLeak", cls, cls, log) {}

        EnvSet leakedAt(Instruction* i) const {
            return at<StaticAnalysis::PositioningStyle::BeforeInstruction>(i);
        }

      protected:
        AbstractResult apply(EnvSet& state, Instruction* i) const override {
            AbstractResult effect;
            if (i->leaksEnv()) {
                if (!state.envs.count(i->env())) {
                    state.envs.insert(i->env());
                    effect.update();
                }
            }
            return effect;
        }
    };

    class ObservedEnvAnalysis : public BackwardStaticAnalysis<EnvSet> {
        const EnvLeakAnalysis& leaked;

      public:
        ObservedEnvAnalysis(ClosureVersion* cls, const CFG& cfg,
                            const EnvLeakAnalysis& leaked, LogStream& log)
            : BackwardStaticAnalysis("observedEnv", cls, cls, cfg, log),
              leaked(leaked) {}

      private:
        static std::unordered_set<Value*> withPotentialParents(Value* env) {
            std::unordered_set<Value*> res;
            assert(env);
            for (;;) {
                if (!MkEnv::Cast(env))
                    break;
                res.insert(env);
                env = Env::parentEnv(env);
            }
            return res;
        }

      protected:
        AbstractResult apply(EnvSet& state, Instruction* i) const override {
            AbstractResult effect;
            if (i->readsEnv() && !StVar::Cast(i)) {
                for (auto& e : withPotentialParents(i->env())) {
                    if (!state.envs.count(e)) {
                        state.envs.insert(e);
                        effect.update();
                    }
                }
            }
            if (i->effects.contains(Effect::ExecuteCode)) {
                auto leakedEnvs = leaked.leakedAt(i);
                for (auto& l : leakedEnvs) {
                    for (auto& e : withPotentialParents(l)) {
                        if (!state.envs.count(e)) {
                            state.envs.insert(e);
                            effect.update();
                        }
                    }
                }
            }
            return effect;
        }

      public:
        bool isObserved(Instruction* pos, Value* env) const {
            return at<PositioningStyle::BeforeInstruction>(pos).envs.count(env);
        }
    };

    EnvLeakAnalysis leak;
    ObservedEnvAnalysis observed;

    static bool isLocal(Value* env) { return MkEnv::Cast(env); }

  public:
    DeadStoreAnalysis(ClosureVersion* cls, const CFG& cfg, LogStream& log)
        : leak(cls, log), observed(cls, cfg, leak, log) {}

    bool isDead(StVar* st) const {
        if (!isLocal(st->env()))
            return false;
        return !observed.isObserved(st, st->env());
    };
};
}
}

#endif
