#ifndef PIR_TRANSLATOR_H
#define PIR_TRANSLATOR_H

#include "../pir/module.h"
#include "../pir/value.h"
#include "runtime/Function.h"

namespace rir {

class PirTranslator {
  public:
    PirTranslator(bool verbose) : verbose(verbose) {}

    pir::Function* compileFunction(SEXP);

    bool isVerbose();
    void setVerbose(bool);
  private:
      bool verbose;
};
}

#endif
