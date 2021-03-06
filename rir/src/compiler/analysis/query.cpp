#include "query.h"
#include "../pir/pir_impl.h"
#include "../util/visitor.h"

namespace rir {
namespace pir {

bool Query::noEnv(Code* c) {
    return Visitor::check(c->entry,
                          [](Instruction* i) { return !MkEnv::Cast(i); });
}

bool Query::pure(Code* c) {
    return Visitor::check(c->entry, [](Instruction* i) {
        return !i->mightIO() && !i->changesEnv();
    });
}

std::unordered_set<Value*> Query::returned(Code* c) {
    std::unordered_set<Value*> returned;
    Visitor::run(c->entry, [&](Instruction* i) {
        Return::Cast(
            i, [&](Return* ret) { returned.insert(ret->arg<0>().val()); });
    });
    return returned;
}
}
}
