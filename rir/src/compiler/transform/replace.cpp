#include "replace.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

void Replace::usesOfValue(Instruction* i, Value* old, Value* rpl) {
    i->eachArg([&](InstrArg& arg) {
        if (arg.val() == old)
            arg.val() = rpl;
    });
}

void Replace::usesOfValue(BB* start, Value* old, Value* rpl) {
    Visitor::run(start, [&](Instruction* i) { usesOfValue(i, old, rpl); });
}
}
}
