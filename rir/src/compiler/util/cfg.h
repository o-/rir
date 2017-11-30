#ifndef PIR_CFG_H
#define PIR_CFG_H

#include "../pir/pir.h"
#include <vector>

namespace rir {
namespace pir {

class CFG {
    typedef std::vector<BB*> BBList;

  public:
    std::vector<BBList> preds;
    BBList exits;

    size_t size() { return preds.size(); }
    CFG(BB*);
};

class DominanceGraph {
    typedef std::vector<BB*> BBList;

  public:
    std::vector<BBList> doms;

    size_t size() { return doms.size(); }
    DominanceGraph(BB*);
};
}
}

#endif
