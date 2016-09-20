#ifndef RIR_OPTIMIZER_CLEANUP_H
#define RIR_OPTIMIZER_CLEANUP_H

#include "code/analysis.h"
#include "code/InstructionVisitor.h"
#include "interpreter/interp_context.h"

#include <unordered_set>

namespace rir {

class BCCleanupV {
  public:
    BCCleanupV(){};
    BCCleanupV(CodeEditor::Cursor c) { defs.insert(c); };

    //    BCCleanupV& operator=(Context what) {
    //        value_ = what;
    //        return *this;
    //    }

    BCCleanupV& operator=(BCCleanupV const& other) = default;

    bool operator==(BCCleanupV const& other) const {
        return defs == other.defs;
    }

    bool operator!=(BCCleanupV const& other) const {
        return defs != other.defs;
    }

    //    Context value() const {
    //        assert(value_ != bottom_ and value_ != top_);
    //        return defs_;
    //    }

    bool mergeWith(BCCleanupV const& other) {
        auto s = defs.size();
        for (auto e : other.defs)
            defs.insert(e);
        return defs.size() != s;
    }

    void insert(CodeEditor::Cursor c) { defs.insert(c); }

    void print() const {}

    std::unordered_set<CodeEditor::Cursor> defs;
};

class BCCleanupA : public ForwardAnalysisIns<AbstractStack<BCCleanupV>>,
                   public InstructionVisitor::Receiver {
  public:
    typedef BCCleanupV Value;
    BCCleanupA() : dispatcher_(*this) {}

  protected:
    virtual Dispatcher& dispatcher() override { return dispatcher_; }

    void push_(CodeEditor::Cursor ins) override {
        current().push(BCCleanupV(ins));
    }

    void label(CodeEditor::Cursor ins) override {}

    /** All other instructions, don't care for now.
     */
    void any(CodeEditor::Cursor ins) override {
        // pop as many as we need, push as many tops as we need
        ins.bc().print();
        current().pop(ins.bc().popCount());
        for (size_t i = 0, e = ins.bc().pushCount(); i != e; ++i)
            current().push(BCCleanupV());
    }

    InstructionVisitor dispatcher_;
};

class BCCleanup : public InstructionVisitor::Receiver {
  public:
    BCCleanupA analysis;
    CodeEditor& code;
    InstructionVisitor dispatcher;

    BCCleanup(CodeEditor& code) : code(code), dispatcher(*this) {}

    void push_(CodeEditor::Cursor ins) override {
        auto v = analysis[ins].top();
        std::cout << v.defs.size() << "\n";
    }

    void run() {
        code.print();
        analysis.analyze(code);
        std::cout << "done\n";
        for (auto i = code.getCursor(); !i.atEnd(); i.advance()) {
            dispatcher.dispatch(i);
        }
    }
};
}
#endif
