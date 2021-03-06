#include "stack_machine.h"
#include "../../analysis/query.h"
#include "../../pir/pir_impl.h"
#include "../../util/builder.h"
#include "R/Funtab.h"
#include "ir/BC.h"
#include "ir/Compiler.h"
#include "rir_2_pir.h"

namespace rir {
namespace pir {

void StackMachine::clear() {
    stack.clear();
    entry = nullptr;
    pc = nullptr;
}

size_t StackMachine::stack_size() { return stack.size(); }

Value* StackMachine::pop() {
    assert(stack_size() > 0);
    auto v = stack.back();
    stack.pop_back();
    return v;
}

Value* StackMachine::top() { return stack.back(); }
Value* StackMachine::front() { return stack.front(); }
void StackMachine::push(Value* v) { stack.push_back(v); }

bool StackMachine::empty() { return stack.empty(); }

Value* StackMachine::at(size_t index) {
    assert(index < stack_size());
    return stack[stack_size() - index - 1];
}

void StackMachine::set(size_t index, Value* value) {
    assert(index < stack_size());
    stack[stack_size() - index - 1] = value;
}

void StackMachine::runCurrentBC(const Rir2Pir& rir2pir, Builder& insert) {
    assert(pc >= srcCode->code() && pc < srcCode->endCode());

    Value* env = insert.env;

    Value* v;
    Value* x;
    Value* y;
    BC bc = this->getCurrentBC();
    switch (bc.bc) {

    case Opcode::push_:
        push(insert(new LdConst(bc.immediateConst())));
        break;

    case Opcode::ldvar_:
        v = insert(new LdVar(bc.immediateConst(), env));
        push(insert(new Force(v)));
        break;

    case Opcode::stvar_:
        v = pop();
        insert(new StVar(bc.immediateConst(), v, env));
        break;

    case Opcode::ldvar_super_:
        insert(new LdVarSuper(bc.immediateConst(), env));
        break;

    case Opcode::stvar_super_:
        v = pop();
        insert(new StVarSuper(bc.immediateConst(), v, env));
        break;

    case Opcode::asbool_:
    case Opcode::aslogical_:
        push(insert(new AsLogical(pop())));
        break;

    case Opcode::ldfun_:
        push(insert(new LdFun(bc.immediateConst(), env)));
        break;

    case Opcode::guard_fun_:
        std::cout << "warn: guard ignored "
                  << CHAR(PRINTNAME(
                         rir::Pool::get(bc.immediate.guard_fun_args.name)))
                  << "\n";
        break;

    case Opcode::swap_:
        x = pop();
        y = pop();
        push(x);
        push(y);
        break;

    case Opcode::dup_:
        push(top());
        break;

    case Opcode::dup2_:
        push(at(1));
        push(at(1));
        break;

    case Opcode::close_: {
        Value* srcref = pop();
        Value* body = pop();
        Value* formals = pop();
        push(insert(new MkCls(formals, body, srcref, env)));
        break;
    }

    case Opcode::nop_:
        break;

    case Opcode::pop_:
        pop();
        break;

    case Opcode::call_: {
        unsigned n = bc.immediate.call_args.nargs;
        rir::CallSite* cs = bc.callSite(srcFunction->body());

        SEXP monomorphic = nullptr;
        if (cs->hasProfile) {
            auto prof = cs->profile();
            if (prof->numTargets == 1) {
                monomorphic = prof->targets[0];
            }
        }

        std::vector<Value*> args;
        for (size_t i = 0; i < n; ++i) {
            unsigned argi = cs->args()[i];
            if (argi == DOTS_ARG_IDX) {
                assert(false);
            } else if (argi == MISSING_ARG_IDX) {
                assert(false);
            }
            rir::Code* promiseCode = srcFunction->codeAt(argi);
            Promise* prom = insert.function->createProm();
            {
                Builder promiseBuilder(insert.function, prom);
                Rir2Pir(rir2pir).compile(promiseCode, promiseBuilder);
            }
            Value* val = Missing::instance();
            if (Query::pure(prom)) {
                val = rir2pir.translate(promiseCode, insert);
            }
            args.push_back(insert(new MkArg(prom, val, env)));
        }

        if (monomorphic && isValidClosureSEXP(monomorphic)) {
            Closure* f = rir2pir.compiler.compileClosure(monomorphic);
            Value* expected = insert(new LdConst(monomorphic));
            Value* t = insert(
                new Eq(top(), expected, 0)); // here we don't have src ast...
            insert(new Branch(t));
            BB* curBB = insert.bb;

            BB* fallback = insert.createBB();
            insert.bb = fallback;
            curBB->next0 = fallback;
            Value* r1 = insert(new Call(insert.env, pop(), args, cs->call));

            BB* asExpected = insert.createBB();
            insert.bb = asExpected;
            curBB->next1 = asExpected;
            Value* r2 = insert(
                new StaticCall(insert.env, f, args, cs->call, monomorphic));

            BB* cont = insert.createBB();
            fallback->next0 = cont;
            asExpected->next0 = cont;
            insert.bb = cont;
            push(insert(new Phi({r1, r2}, {fallback, asExpected})));
        } else {
            push(insert(new Call(insert.env, pop(), args, cs->call)));
        }
        break;
    }

    case Opcode::promise_: {
        unsigned promi = bc.immediate.i;
        rir::Code* promiseCode = srcFunction->codeAt(promi);
        Promise* prom = insert.function->createProm();
        {
            // What should I do with this?
            Builder promiseBuilder(insert.function, prom);
            Rir2Pir(rir2pir).compile(promiseCode, promiseBuilder);
        }
        Value* val = Missing::instance();
        if (Query::pure(prom)) {
            val = rir2pir.translate(promiseCode, insert);
        }
        // TODO: Remove comment and check how to deal with
        push(insert(new MkArg(prom, val, env)));
        break;
    }

    case Opcode::static_call_stack_: {
        unsigned n = bc.immediate.call_args.nargs;
        rir::CallSite* cs = bc.callSite(srcFunction->body());
        SEXP target = rir::Pool::get(*cs->target());

        std::vector<Value*> args(n);
        for (size_t i = 0; i < n; ++i)
            args[n - i - 1] = pop();

        if (TYPEOF(target) == BUILTINSXP) {
            // TODO: compile a list of safe builtins
            static int vector = findBuiltin("vector");

            if (getBuiltinNr(target) == vector)
                push(insert(new CallSafeBuiltin(target, args, cs->call)));
            else
                push(insert(new CallBuiltin(env, target, args, cs->call)));
        } else {
            assert(TYPEOF(target) == CLOSXP);
            if (!isValidClosureSEXP(target)) {
                target = Compiler::compileClosure(target);
                // TODO: we need to keep track of this compiled rir function.
                // For now let's just put it in the constant pool.
                Pool::insert(target);
            }
            Closure* f = rir2pir.compiler.compileClosure(target);
            push(insert(new StaticEagerCall(env, f, args, cs->call, target)));
        }
        break;
    }

    case Opcode::seq_: {
        auto step = pop();
        auto stop = pop();
        auto start = pop();
        push(insert(new Seq(start, stop, step)));
        break;
    }

    case Opcode::for_seq_size_:
        push(insert(new ForSeqSize(top())));
        break;

    case Opcode::extract1_1_: {
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract1_1D(vec, idx)));
        break;
    }

    case Opcode::extract2_1_: {
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Extract2_1D(vec, idx)));
        break;
    }

    case Opcode::extract1_2_: {
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract1_2D(vec, idx1, idx2)));
        break;
    }

    case Opcode::extract2_2_: {
        Value* idx2 = pop();
        Value* idx1 = pop();
        Value* vec = pop();
        push(insert(new Extract2_2D(vec, idx1, idx2)));
        break;
    }

    case Opcode::subassign1_: {
        Value* val = pop();
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Subassign1_1D(vec, idx, val)));
        break;
    }

    case Opcode::subassign2_: {
        SEXP sym = rir::Pool::get(bc.immediate.pool);
        Value* val = pop();
        Value* idx = pop();
        Value* vec = pop();
        push(insert(new Subassign2_1D(vec, idx, val, sym)));
        break;
    }

#define BINOP(Name, Op)                                                        \
    case Opcode::Op: {                                                         \
        auto rhs = pop();                                                      \
        auto lhs = pop();                                                      \
        push(insert(new Name(lhs, rhs, getSrcIdx())));                         \
        break;                                                                 \
    }
        BINOP(LOr, lgl_or_);
        BINOP(LAnd, lgl_and_);
        BINOP(Lt, lt_);
        BINOP(Gt, gt_);
        BINOP(Gte, le_);
        BINOP(Lte, ge_);
        BINOP(Mod, mod_);
        BINOP(Div, div_);
        BINOP(IDiv, idiv_);
        BINOP(Add, add_);
        BINOP(Mul, mul_);
        BINOP(Colon, colon_);
        BINOP(Pow, pow_);
        BINOP(Sub, sub_);
        BINOP(Eq, eq_);
        BINOP(Neq, ne_);
#undef BINOP

#define UNOP(Name, Op)                                                         \
    case Opcode::Op: {                                                         \
        v = pop();                                                             \
        push(insert(new Name(v)));                                             \
        break;                                                                 \
    }
        UNOP(Plus, uplus_);
        UNOP(Minus, uminus_);
        UNOP(Inc, inc_);
        UNOP(Not, not_);
        UNOP(Length, length_);
#undef UNOP

    case Opcode::is_:
        push(insert(new Is(bc.immediate.i, pop())));
        break;

    case Opcode::pull_: {
        size_t i = bc.immediate.i;
        push(at(i));
        break;
    }

    case Opcode::pick_: {
        x = at(bc.immediate.i);
        for (int i = bc.immediate.i; i > 0; --i)
            set(i, at(i - 1));
        set(0, x);
        break;
    }

    case Opcode::put_: {
        x = top();
        for (size_t i = 0; i < bc.immediate.i - 1; ++i)
            set(i, at(i + 1));
        set(bc.immediate.i, x);
        break;
    }

    // TODO implement!
    // (silently ignored)
    case Opcode::set_shared_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::isfun_:
    case Opcode::make_unique_:
    case Opcode::brobj_:
        break;

    // Currently unused opcodes:
    case Opcode::alloc_:
    case Opcode::push_code_:
    case Opcode::set_names_:
    case Opcode::names_:
    case Opcode::force_:

    // Invalid opcodes:
    case Opcode::label:
    case Opcode::invalid_:
    case Opcode::num_of:

    // Opcodes handled elsewhere
    case Opcode::brtrue_:
    case Opcode::brfalse_:
    case Opcode::br_:
    case Opcode::ret_:
    case Opcode::return_:
        assert(false);

    // Opcodes that only come from PIR
    case Opcode::make_env_:
    case Opcode::get_env_:
    case Opcode::set_env_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::ldarg_:
    case Opcode::ldloc_:
    case Opcode::stloc_:
    case Opcode::movloc_:
    case Opcode::isobj_:
    case Opcode::check_missing_:
        assert(false && "Recompiling PIR not supported for now.");

    // Unsupported opcodes:
    case Opcode::ldlval_:
    case Opcode::asast_:
    case Opcode::missing_:
    case Opcode::dispatch_stack_:
    case Opcode::dispatch_:
    case Opcode::guard_env_:
    case Opcode::call_stack_:
    case Opcode::beginloop_:
    case Opcode::endcontext_:
    case Opcode::ldddvar_:
    case Opcode::int3_:
        std::cerr << "Cannot compile Function. Unsupported bc\n";
        bc.print();
        assert(false);
        break;
    }
}

bool StackMachine::doMerge(Opcode* trg, Builder& builder, StackMachine* other) {
    if (other->entry == nullptr) {
        other->entry = builder.createBB();
        other->pc = trg;
        for (size_t i = 0; i < stack_size(); ++i) {
            auto v = stack.at(i);
            auto p = new Phi;
            other->entry->append(p);
            p->addInput(builder.bb, v);
            other->push(p);
        }

        return true;
    }

    assert(stack_size() == other->stack_size());

    for (size_t i = 0; i < stack_size(); ++i) {
        Phi* p = Phi::Cast(other->stack.at(i));
        assert(p);
        Value* incom = stack.at(i);
        if (incom != p) {
            p->addInput(builder.bb, incom);
        }
    }
    return false;
}

Opcode* StackMachine::getPC() { return pc; }

void StackMachine::setPC(Opcode* opcode) { pc = opcode; }

pir::BB* StackMachine::getEntry() { return entry; }
void StackMachine::setEntry(pir::BB* ent) { entry = ent; }

void StackMachine::advancePC() { BC::advance(&pc); }

BC StackMachine::getCurrentBC() { return BC::decode(pc); }

unsigned StackMachine::getSrcIdx() { return srcCode->getSrcIdxAt(pc, true); }

} // namespace pir
} // namespace rir
