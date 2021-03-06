#ifndef COMPILER_VALUE_H
#define COMPILER_VALUE_H

#include "type.h"

#include <functional>
#include <iostream>

namespace rir {
namespace pir {

enum class Tag : uint8_t;

class BB;

/*
 * A typed PIR value.
 *
 * Has a tag from either value_list.h or instruction_list.h
 *
 */
class Value {
  public:
    PirType type;
    Tag tag;
    Value(PirType type, Tag tag) : type(type), tag(tag) {}
    void printRef(std::ostream& out);
    void printRef() { printRef(std::cerr); }
    bool isInstruction();
};

}
}

#endif
