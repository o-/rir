#ifndef RIR_2_PIR_H
#define RIR_2_PIR_H

#include "pir_translator.h"
#include "../pir/stack_machine.h"
#include <unordered_map>

namespace rir {

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
        : PirTranslator(cmp.isVerbose()), insert(insert), cmp(cmp),
          srcFunction(srcFunction), srcCode(srcCode) {}

    pir::Value* translate();

    typedef pir::StackMachine::ReturnSite ReturnSite;
    void addReturn(ReturnSite r) { results.push_back(r); }

    Rir2PirCompiler& compiler() { return cmp; }

  private:
    bool done = false;

    pir::Builder& insert;
    Rir2PirCompiler& cmp;

    rir::Function* srcFunction;
    rir::Code* srcCode;

    std::unordered_map<Opcode*, pir::StackMachine> mergepoint;
    std::vector<ReturnSite> results;

    void recoverCFG(rir::Code*);
    bool doMerge(Opcode* trg);
    void compileReturn(pir::Value*);

    friend class RirInlinedPromise2Rir;
};
}
#endif
