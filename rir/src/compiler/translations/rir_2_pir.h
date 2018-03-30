#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "pir_translator.h"
#include "../pir/stack_machine.h"
#include <unordered_map>

namespace rir {

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

class Rir2PirCompiler {
  public:
    Rir2PirCompiler(pir::Module* module) : module(module) {}
    pir::Function* compileFunction(SEXP);
    pir::Function* compileFunction(Function*, std::vector<SEXP>);
    void optimizeModule();
    pir::Module* getModule() { return module; }

    bool isVerbose() { return verbose; }
    void setVerbose(bool v) { verbose = v; }

  private:
    bool verbose = false;
    pir::Module* module;
};

class Rir2Pir : public PirTranslator {
  public:
    Rir2Pir(Rir2PirCompiler& cmp, pir::Builder& insert,
            rir::Function* srcFunction, rir::Code* srcCode)
        : PirTranslator(cmp.isVerbose()), cmp(cmp), insert(insert),
          srcFunction(srcFunction), srcCode(srcCode) {}
    pir::Value* translate();

    /*static pir::IRTransformation* declare(SEXP&);
    static pir::IRTransformation* declare(rir::Function*);
    static pir::IRTransformation* declare(rir::Function*, rir::Code*);*/

  private:
    Rir2PirCompiler& cmp;
    pir::Builder& insert;

    rir::Function* srcFunction;
    rir::Code* srcCode;

    std::unordered_map<Opcode*, pir::StackMachine> mergepoint;
    pir::StackMachine state;

    void recoverCFG(rir::Code*);
    bool doMerge(Opcode* trg);
    void popFromWorklist(std::deque<pir::StackMachine>*);
    void addReturn(pir::Value*);

    friend class pir::StackMachine;
    friend class RirInlinedPromise2Rir;
};
}
#endif
