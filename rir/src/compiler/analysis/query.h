#ifndef COMPILER_PIR_QUERY_H
#define COMPILER_PIR_QUERY_H

#include "../pir/pir.h"

namespace rir {
namespace pir {

/*
 * Simple queries, that should be O(n) to compute
 *
 */
class Query {
  public:
    static bool pure(Code* c);
};
}
}

#endif
