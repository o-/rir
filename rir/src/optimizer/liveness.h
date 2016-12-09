#pragma once

#include <algorithm>
#include <unordered_set>

#include "../ir/CodeEditor.h"
#include "../code/framework.h"
#include "../code/analysis.h"


namespace rir {


/** Our abstract value.
 *
 * Liveness analysis uses powerset lattice, the elements being the variables of the analyzed function.
 * At all points in the function we have one abstract value which tells the set of variables that are
 * live at that point, i.e. their values are read by someone later.
 */

class ASet {
public:

    ASet() = default;
    ASet(ASet const & other) = default;

    static ASet const & Absent() { return bottom(); }

    bool mergeWith(ASet const & other) {
        // This is just set union

        auto originalSize = variables.size();

        for (auto const & var : other.variables) {
            variables.insert(var);
        }

        return originalSize != variables.size();
    }

    // Bottom is empty set
    static ASet const & bottom() {
        static ASet value;
        return value;
    }

    void print() const {
        Rprintf("{ ");
        for (auto const & var : variables) {
            Rprintf("%s ", var.c_str());
        }
        Rprintf("}");
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
        // No instructions use stack, we only have one abstract value stored here
        result->push(ASet::bottom());
        return result;
    }

    void ldarg_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().top().variables.insert(CHAR(PRINTNAME((bc.immediateConst()))));
    }

    void ldvar_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().top().variables.insert(CHAR(PRINTNAME((bc.immediateConst()))));
    }

    void stvar_(CodeEditor::Iterator ins) override {
        BC bc = *ins;
        current().top().variables.erase(CHAR(PRINTNAME((bc.immediateConst()))));
    }

    void ret_(CodeEditor::Iterator ins) override {
    }

    void return_(CodeEditor::Iterator ins) override {
    }

    void any(CodeEditor::Iterator ins) override {}

    Dispatcher & dispatcher() override {
        return dispatcher_;
    }

private:
    InstructionDispatcher dispatcher_;

};


}  // namespace rir
