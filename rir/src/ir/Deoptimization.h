#ifndef RIR_DEOPTIMIZATION_H
#define RIR_DEOPTIMIZATION_H

#include <iostream>

namespace rir {
#pragma pack(push)
#pragma pack(1)

struct Code;

struct FrameInfo {
    uint8_t* pc;
    Code* code;
    size_t stackSize;
};

struct DeoptMetadata {
    void print(std::ostream& out) const;
    size_t numFrames;
    FrameInfo frames[];
};

#pragma pack(pop)
} // namespace rir

#endif
