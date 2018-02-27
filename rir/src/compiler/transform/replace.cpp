#include "replace.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

void Replace::usesOfValue(Instruction* i, Value* old, Value* rpl) {
    i->map_arg([&](Value** v) {
        if (*v == old)
            *v = rpl;
        Value* vrpl = (*v)->replaceRefs(old, rpl);
        if (vrpl != *v)
            *v = vrpl;
    });
}

void Replace::usesOfValue(BB* start, Value* old, Value* rpl) {
    Visitor::run(start, [&](BB* bb) {
        for (auto i : *bb) {
            usesOfValue(i, old, rpl);
        }
    });
}
}
}
