#include "jit_llvm.h"

#include "types_llvm.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/IndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Vectorize.h"

// analysis passes
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Vectorize.h>

#include <llvm/Support/SmallVectorMemoryBuffer.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Bitcode/BitcodeWriterPass.h>

#include "llvm/Object/ArchiveWriter.h"
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/LegacyPassManagers.h>
#include <llvm/Transforms/Utils/Cloning.h>

namespace {

using namespace llvm;
using namespace llvm::orc;

static LLVMContext C;

class JitLLVMImplementation {
  private:
    ExecutionSession ES;
    std::shared_ptr<SymbolResolver> Resolver;
    std::unique_ptr<TargetMachine> TM;
    DataLayout DL;
    LegacyRTDyldObjectLinkingLayer ObjectLayer;
    LegacyIRCompileLayer<decltype(ObjectLayer), SimpleCompiler> CompileLayer;

    using OptimizeFunction = std::function<std::unique_ptr<llvm::Module>(
        std::unique_ptr<llvm::Module>)>;

    LegacyIRTransformLayer<decltype(CompileLayer), OptimizeFunction>
        OptimizeLayer;
    MangleAndInterner Mangle;

  public:
    JitLLVMImplementation()
        : Resolver(createLegacyLookupResolver(
              ES,
              [this](const std::string& Name) -> JITSymbol {
                  if (auto Sym = OptimizeLayer.findSymbol(Name, false))
                      return Sym;
                  else if (auto Err = Sym.takeError())
                      return std::move(Err);
                  if (auto SymAddr =
                          RTDyldMemoryManager::getSymbolAddressInProcess(Name))
                      return JITSymbol(SymAddr, JITSymbolFlags::Exported);
                  return nullptr;
              },
              [](Error Err) {
                  cantFail(std::move(Err), "lookupFlags failed");
              })),
          TM(EngineBuilder().selectTarget()), DL(TM->createDataLayout()),
          ObjectLayer(ES,
                      [this](VModuleKey K) {
                          return LegacyRTDyldObjectLinkingLayer::Resources{
                              std::make_shared<SectionMemoryManager>(),
                              Resolver};
                      }),
          CompileLayer(ObjectLayer, SimpleCompiler(*TM)),
          OptimizeLayer(CompileLayer,
                        [this](std::unique_ptr<llvm::Module> M) {
                            return optimizeModule(std::move(M));
                        }),
          Mangle(ES, this->DL) {
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
    }

    llvm::Module* module;

    void* tryCompile(llvm::Function* fun) {
        module = new llvm::Module(fun->getName(), C);
        module->setDataLayout(TM->createDataLayout());

        verifyFunction(*fun);

        auto K = ES.allocateVModule();
        cantFail(
            OptimizeLayer.addModule(K, std::unique_ptr<llvm::Module>(module)));
        auto res = OptimizeLayer.findSymbol(fun->getName(), true);
        auto adr = res.getAddress();
        cantFail(OptimizeLayer.removeModule(K));
        if (adr) {
            return (void*)*adr;
        }
        return nullptr;
    }

    static JitLLVMImplementation& instance() {
        static std::unique_ptr<JitLLVMImplementation> singleton;
        if (!singleton) {
            InitializeNativeTarget();
            InitializeNativeTargetAsmPrinter();
            InitializeNativeTargetAsmParser();
            rir::pir::initializeTypes(C);
            singleton = std::make_unique<JitLLVMImplementation>();
        }
        return *singleton;
    }

  private:
    std::unique_ptr<llvm::Module>
    optimizeModule(std::unique_ptr<llvm::Module> M) {
        // Create a function pass manager.
        auto PM = llvm::make_unique<legacy::FunctionPassManager>(M.get());

        PM->add(createScopedNoAliasAAWrapperPass());
        PM->add(createTypeBasedAAWrapperPass());
        PM->add(createBasicAAWrapperPass());

        // list of passes from vmkit
        PM->add(createCFGSimplificationPass()); // Clean up disgusting code
        PM->add(createDeadCodeEliminationPass());
        PM->add(createSROAPass()); // Kill useless allocas

        PM->add(createMemCpyOptPass());

        // Running `memcpyopt` between this and `sroa` seems to give `sroa` a
        // hard time merging the `alloca` for the unboxed data and the `alloca`
        // created by the `alloc_opt` pass.
        PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
        // Now that SROA has cleaned up for front-end mess, a lot of control
        // flow should be more evident - try to clean it up.
        PM->add(createCFGSimplificationPass());    // Merge & remove BBs
        PM->add(createSROAPass());                 // Break up aggregate allocas
        PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
        PM->add(createJumpThreadingPass());        // Thread jumps.
        PM->add(createInstructionCombiningPass()); // Combine silly seq's

        PM->add(createCFGSimplificationPass()); // Merge & remove BBs
        PM->add(createReassociatePass());       // Reassociate expressions

        PM->add(createEarlyCSEPass()); //// ****

        // Load forwarding above can expose allocations that aren't actually
        // used remove those before optimizing loops.
        PM->add(createLoopIdiomPass());  //// ****
        PM->add(createLoopRotatePass()); // Rotate loops.
        // LoopRotate strips metadata from terminator, so run LowerSIMD
        // afterwards
        PM->add(createLICMPass());         // Hoist loop invariants
        PM->add(createLoopUnswitchPass()); // Unswitch loops.
        // Subsequent passes not stripping metadata from terminator
        PM->add(createInstructionCombiningPass());
        PM->add(createIndVarSimplifyPass());     // Canonicalize indvars
        PM->add(createLoopDeletionPass());       // Delete dead loops
        PM->add(createSimpleLoopUnrollPass());   // Unroll small loops
        PM->add(createLoopStrengthReducePass()); // (jwb added)

        // Re-run SROA after loop-unrolling (useful for small loops that
        // operate, over the structure of an aggregate)
        PM->add(createSROAPass()); // Break up aggregate allocas
        PM->add(
            createInstructionCombiningPass()); // Clean up after the unroller
        PM->add(createGVNPass());              // Remove redundancies
        PM->add(createMemCpyOptPass());        // Remove memcpy / form memset
        PM->add(createSCCPPass());             // Constant prop with SCCP

        // Run instcombine after redundancy elimination to exploit opportunities
        // opened up by them.
        PM->add(createSinkingPass()); ////////////// ****
        PM->add(createInstructionCombiningPass());
        PM->add(createJumpThreadingPass());        // Thread jumps
        PM->add(createDeadStoreEliminationPass()); // Delete dead stores

        // see if all of the constant folding has exposed more loops
        // to simplification and deletion
        // this helps significantly with cleaning up iteration
        PM->add(createCFGSimplificationPass()); // Merge & remove BBs
        PM->add(createLoopIdiomPass());
        PM->add(createLoopDeletionPass());  // Delete dead loops
        PM->add(createJumpThreadingPass()); // Thread jumps
        PM->add(createSLPVectorizerPass());
        PM->add(createAggressiveDCEPass()); // Delete dead instructions
        PM->add(createInstructionCombiningPass());
        PM->add(createLoopVectorizePass());
        PM->add(createInstructionCombiningPass());

        PM->doInitialization();

        // Run the optimizations over all functions in the module being added to
        // the JIT.
        for (auto& F : *M) {
            PM->run(F);
            F.dump();
        }

        return M;
    }
};
} // namespace

namespace rir {
namespace pir {

void* JitLLVM::tryCompile(llvm::Function* fun) {
    return JitLLVMImplementation::instance().tryCompile(fun);
}

llvm::Function* JitLLVM::declare(const std::string& name,
                                 llvm::FunctionType* signature) {
    return Function::Create(signature, Function::ExternalLinkage, name,
                            JitLLVMImplementation::instance().module);
}

} // namespace pir
} // namespace rir
