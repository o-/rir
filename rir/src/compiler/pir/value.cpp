#include "value.h"
#include "pir_impl.h"

namespace rir {
namespace pir {

void Value::printRef(std::ostream& out) {
    switch (tag) {
#define V(Name)                                                                \
    case Tag::Name:                                                            \
        static_cast<Name*>(this)->printRef(out);                               \
        break;
        COMPILER_INSTRUCTIONS(V);
#undef V
#define V(Name)                                                                \
    case Tag::Name:                                                            \
        static_cast<Name*>(this)->printRef(out);                               \
        break;
        COMPILER_VALUES(V);
#undef V
    case Tag::_UNUSED_:
        assert(false);
    };
}

bool Value::isInstruction() {
    switch (tag) {
#define V(Name) case Tag::Name:
        COMPILER_INSTRUCTIONS(V);
        return true;
#undef V
#define V(Name) case Tag::Name:
        COMPILER_VALUES(V);
        return false;
#undef V
    case Tag::_UNUSED_:
        assert(false);
    };
    assert(false);
    return false;
}

}
}
