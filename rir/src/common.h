#ifndef RIR_COMMON_H
#define RIR_COMMON_H

#include <cassert>
#include <cstdint>

// TODO force inlining for clang & gcc
#define RIR_INLINE __attribute__((always_inline)) inline

extern void printCBacktrace();
extern void printRBacktrace();
extern void printBacktrace();

#ifdef ENABLE_SLOWASSERT
#define SLOWASSERT(what) assert(what)
#else
#define SLOWASSERT(what)                                                       \
    {}
#endif

#endif
