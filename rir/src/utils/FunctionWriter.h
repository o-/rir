#ifndef RIR_FUNCTION_HANDLE_H
#define RIR_FUNCTION_HANDLE_H

#include "interpreter/instance.h"
#include "ir/BC_inc.h"

#include "R/Preserve.h"
#include "ir/CodeVerifier.h"
#include "runtime/Function.h"
#include "utils/Pool.h"

#include <iostream>
#include <map>
#include <vector>

namespace rir {

class FunctionWriter {
  private:
    Function* function_;
    std::vector<SEXP> defaultArgs;
    Preserve preserve;

  public:
    typedef unsigned PcOffset;

    FunctionWriter() : function_(nullptr) {}

    ~FunctionWriter() {}

    Function* function() {
        assert(function_ && "FunctionWriter has not been finalized");
        return function_;
    }

    void addArgWithoutDefault() { defaultArgs.push_back(nullptr); }

    void addDefaultArg(Code* code) {
        preserve(code->container());
        defaultArgs.push_back(code->container());
    }

    void finalize(Code* body, const FunctionSignature& signature) {
        assert(function_ == nullptr && "Trying to finalize a second time");

        size_t dataSize = defaultArgs.size() * sizeof(SEXP);
        size_t functionSize = sizeof(Function) + dataSize;

        SEXP store = Rf_allocVector(EXTERNALSXP, functionSize);
        void* payload = INTEGER(store);
        Function* fun = new (payload)
            Function(functionSize, body->container(), defaultArgs, signature);
        preserve(store);

        assert(fun->info.magic == FUNCTION_MAGIC);

        function_ = fun;
    }

    Code* writeCode(SEXP ast, uint8_t* bc, unsigned originalCodeSize,
                    const std::map<PcOffset, BC::PoolIdx>& sources,
                    const std::map<PcOffset, BC::Label>& patchpoints,
                    const std::map<PcOffset, std::vector<BC::Label>>& labels,
                    size_t localsCnt, size_t erased) {
        assert(function_ == nullptr &&
               "Trying to add more code after finalizing");
        unsigned codeSize = originalCodeSize - erased;
        unsigned totalSize = Code::size(codeSize, sources.size());

        SEXP store = Rf_allocVector(EXTERNALSXP, totalSize);
        void* payload = DATAPTR(store);
        Code* code = new (payload)
            Code(nullptr, ast, codeSize, sources.size(), localsCnt);
        preserve(store);

        size_t numberOfSources = 0;

        // Since we are removing instructions from the BC stream, we need to
        // update labels and patchpoint offsets.
        std::vector<PcOffset> updatedLabel2Pos;
        std::unordered_map<PcOffset, BC::Label> updatedPatchpoints;
        {
            uint8_t* from = bc;
            uint8_t* to = code->code();
            const uint8_t* from_start = bc;
            const uint8_t* to_start = code->code();
            const uint8_t* from_end = from + originalCodeSize;
            const uint8_t* to_end = to + codeSize;

            // Since those are ordered maps, the elements appear in order. Our
            // strategy is thus, to wait for the next element to show up in the
            // source stream, and transfer them to the updated maps for the
            // target code stream.
            auto source = sources.begin();
            auto patchpoint = patchpoints.begin();
            auto label = labels.begin();

            while (from != from_end) {
                assert(to < to_start + codeSize);

                // We skip erased instructions
                while (*from == 0) {
                    erased--;
                    from++;
                    continue;
                }

                unsigned bcSize = BC::size(from);
                PcOffset fromOffset = from - from_start;
                PcOffset fromOffsetAfter = fromOffset + bcSize;
                PcOffset toOffset = to - to_start;

                // Look for labels. If we have a label in the 'from' stream,
                // when we will need to note the position of that label in the
                // 'to' stream.
                if (label != labels.end()) {
                    auto nextLabelPos = label->first;
                    assert(nextLabelPos >= fromOffset);
                    if (nextLabelPos == fromOffset) {
                        for (auto labelNr : label->second) {
                            if ((unsigned)labelNr >= updatedLabel2Pos.size())
                                updatedLabel2Pos.resize(labelNr + 1, -1);
                            updatedLabel2Pos[labelNr] = toOffset;
                        }
                        label++;
                    }
                }

                // Copy the bytecode from 'from' to 'to'
                memcpy(to, from, bcSize);

                // The code stream stores sources after the instruction, but in
                // the BC we actually need the index before the instruction.
                // If the current BC in the code stream has a source attached,
                // we add it to the sources list of the code object.
                if (source != sources.end()) {
                    assert(source->first >= fromOffsetAfter);
                    if (source->first == fromOffsetAfter) {
                        code->srclist()[numberOfSources].pcOffset = toOffset;
                        code->srclist()[numberOfSources].srcIdx =
                            source->second;
                        numberOfSources++;
                        source++;
                    }
                }

                // Patchpoints can occur anywhere within BCs. If there is a
                // patchpoint in the 'from' BC, we need to update it, such
                // that it references the correct place in the 'to' BC.
                if (patchpoint != patchpoints.end()) {
                    auto patchpointPos = patchpoint->first;
                    assert(patchpointPos >= fromOffset);
                    auto patchpointDistance = patchpointPos - fromOffset;
                    if (patchpointDistance < bcSize) {
                        updatedPatchpoints[toOffset + patchpointDistance] =
                            patchpoint->second;
                        patchpoint++;
                    }
                }

                from += bcSize;
                to += bcSize;
            }

            // Make sure that there is no dangling garbage at the end, if we
            // skipped more instructions than anticipated
            while (to != to_end) {
                *to++ = 0;
            }

            assert(to == to_end);
        }
        assert(erased == 0 && "Client reported wrong number of erased bytes");
        assert(patchpoints.size() == updatedPatchpoints.size());

        // Patch jumps with actual offset in bytes
        for (auto p : updatedPatchpoints) {
            unsigned pos = p.first;
            unsigned labelNr = p.second;
            assert(labelNr < updatedLabel2Pos.size() &&
                   "Jump to missing label");
            unsigned target = updatedLabel2Pos[labelNr];
            assert(target != (unsigned)-1 && "Jump to missing label");
            BC::Jmp j = target - pos - sizeof(BC::Jmp);
            *(BC::Jmp*)((uintptr_t)code->code() + pos) = j;
        }

        assert(numberOfSources == sources.size());

        return code;
    }
};
}

#endif
