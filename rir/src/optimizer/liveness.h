#pragma once

#include <algorithm>
#include <unordered_set>

#include "../ir/CodeEditor.h"
#include "../code/framework.h"
#include "../code/analysis.h"


namespace rir {


/** Our abstract value.
 */

class ASet {
public:

    ASet(ASet const & other)  = default;

    static ASet const & Absent() { return top(); }

    bool mergeWith(ASet const & other) {
        bool result = false;
        assert(false && "Not yet implemented.");
        return result;
    }

    // TODO constructors
    ASet() = default;

    static ASet const & top() {
        assert(false && "Not yet implemented.");
        static ASet value;
        return value;
    }

    static ASet const & bottom() {
        static ASet value;
        return value;
    }

    void print() const {
        assert(false && "Not yet implemented.");
    }

    std::unordered_set<std::string> variables;

};

class LivenessAnalysis : public BackwardAnalysisIns<AbstractState<ASet>>, public InstructionDispatcher::Receiver {
public:
    LivenessAnalysis() :
        dispatcher_(*this) {
    }

protected:

    AbstractState<ASet> * initialState() override {
        auto * result = new AbstractState<ASet>();
        return result;
    }

    void any(CodeEditor::Iterator ins) override {
       assert(false && "Not yet implemented.");
    }

    // TODO more instructions...

    Dispatcher & dispatcher() override {
        return dispatcher_;
    }

private:
    InstructionDispatcher dispatcher_;

};


}  // namespace rir
