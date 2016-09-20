#ifndef RIR_OPTIMIZER_CLEANUP_H
#define RIR_OPTIMIZER_CLEANUP_H

#include "code/dataflow.h"

namespace rir {

class BCCleanup : public InstructionVisitor::Receiver {
  public:
    DataflowAnalysis analysis;
    CodeEditor& code;
    InstructionVisitor dispatcher;

    BCCleanup(CodeEditor& code) : code(code), dispatcher(*this) {}

    class NoSideeffect : public InstructionVisitor::Receiver {
      public:
        bool ans = false;
        void push_(CodeEditor::Cursor ins) override { ans = true; }
        void dup_(CodeEditor::Cursor ins) override { ans = true; }
    };

    void pop_(CodeEditor::Cursor ins) override {
        auto v = analysis[ins].top();
        if (v.uses.empty() && v.defs.size() == 1) {
            for (auto def : v.defs) {
                NoSideeffect noSideeffect;
                InstructionVisitor noSideeffectD(noSideeffect);
                noSideeffectD.dispatch(def);
                if (noSideeffect.ans) {
                    def.remove();
                    ins.remove();
                }
            }
        }
    }

    void run() {
        std::cout << "------ BEFORE ---------------\n";
        code.print();
        analysis.analyze(code);
        for (auto i = code.getCursor(); !i.atEnd(); i.advance()) {
            dispatcher.dispatch(i);
        }
        std::cout << "------ AFTER  ---------------\n";
        code.print();
    }
};
}
#endif
