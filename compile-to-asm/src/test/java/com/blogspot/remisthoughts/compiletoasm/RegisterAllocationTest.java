package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.copySolutionIntoCode;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.getLPproblem;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.move;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.noNoOps;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.ret;
import static com.blogspot.remisthoughts.compiletoasm.DebugUtils.printRegisterLife;
import static com.blogspot.remisthoughts.compiletoasm.DebugUtils.printShouldBeSwitches;
import static com.blogspot.remisthoughts.compiletoasm.DebugUtils.printVariableLife;
import static com.blogspot.remisthoughts.compiletoasm.TestUtils.assertAllAssigned;
import static com.google.common.collect.Iterables.filter;
import static com.google.common.collect.Iterables.isEmpty;
import static org.gnu.glpk.GLPK.glp_init_iocp;
import static org.gnu.glpk.GLPK.glp_intopt;
import static org.gnu.glpk.GLPKConstants.GLP_ON;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.gnu.glpk.glp_iocp;
import org.gnu.glpk.glp_prob;
import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.AtAddress;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Call;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ControlFlowGraph;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Definition;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Label;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ParsingState;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public class RegisterAllocationTest {

	private static List<Instruction> assertAllocation(List<Instruction> code) {
		ControlFlowGraph cfg = new ControlFlowGraph(code, new Variable());
		glp_prob problem = solve(cfg, code);
		code = afterSolve(code, cfg, problem);

		code = Lists.newArrayList(filter(code, noNoOps));
		for (Instruction i : code) {
			assertTrue("expected '" + i + "' to have no variables", isEmpty(i.uses(Variable.class)));
		}
		System.out.println(Joiner.on('\n').join(code));
		return code;
	}

	@Test
	public void testNoAssignment() throws Exception {
		Variable a = new Variable("a");
		Variable b = new Variable("b");
		Immediate one = new Immediate(1);

		/*
		 * a = 1;
		 * a = +(a, 1);
		 * return a;
		 */
		List<Instruction> code = Arrays.asList(
				new Definition(new Label("one_plus_one"), false),
				move(one, a),
				move(a, b),
				Op.addq.with(one, b),
				move(b, a),
				move(a, Register.rax),
				ret);

		code = assertAllocation(code);
		assertEquals(4, code.size());
	}

	@Test
	public void testSpillage() throws Exception {
		/*
		 * _main:
		 * movq %rbx, save!%rbx
		 * movq %rbp, save!%rbp
		 * movq %r12, save!%r12
		 * movq %r13, save!%r13
		 * movq %r14, save!%r14
		 * movq %r15, save!%r15
		 * movq %rdi, a
		 * movq $0x3, VAR0
		 * addq $0x2, VAR0
		 * movq VAR0, (a)
		 * movq (a), ret
		 * movq a, %rdi
		 * call _free
		 * movq ret, %rax
		 * movq save!%rbx, %rbx
		 * movq save!%rbp, %rbp
		 * movq save!%r12, %r12
		 * movq save!%r13, %r13
		 * movq save!%r14, %r14
		 * movq save!%r15, %r15
		 * ret
		 */
		Variable a = new Variable("a"), ret = new Variable("ret"), var0 = new Variable("VAR0"), save_rbx = new Variable("save!%rbx"), save_rbp = new Variable("save!%rbp"), save_r12 = new Variable("save!%r12"), save_r13 = new Variable("save!%r13"), save_r14 = new Variable("save!%r14"), save_r15 = new Variable("save!%r15");
		Immediate two = new Immediate(2), three = new Immediate(3);
		AtAddress atA = new AtAddress(a, 0);

		List<Instruction> code = Lists.newArrayList(
				new Definition(new Label("_main"), false),
				move(Register.rbx, save_rbx),
				move(Register.rbp, save_rbp),
				move(Register.r12, save_r12),
				move(Register.r13, save_r13),
				move(Register.r14, save_r14),
				move(Register.r15, save_r15),
				move(Register.rdi, a),
				move(three, var0),
				Op.addq.with(two, var0),
				move(var0, atA),
				move(atA, ret),
				move(a, Register.rdi),
				new Call(new Label("_free")),
				move(ret, Register.rax),
				move(save_rbx, Register.rbx),
				move(save_rbp, Register.rbp),
				move(save_r12, Register.r12),
				move(save_r13, Register.r13),
				move(save_r14, Register.r14),
				move(save_r15, Register.r15),
				Compiler.ret);

		// assigns: ret->rbp, a->rdi, var0->r10
		ControlFlowGraph cfg = new ControlFlowGraph(code, new Variable());
		glp_prob lpProblem = solve(cfg, code);

		// some debug printing
		printVariableLife(lpProblem, cfg, code, save_rbp);
		printRegisterLife(lpProblem, cfg, code, Register.rbp);
		// printShouldBeSwitches(lpProblem, cfg, code);
		// System.out.println(lpProblem.objective().constraintToString().replaceAll(" \\+ ", " \n\\+ ").replaceAll(" - ", " \n- ").replaceAll(": ", ":\n"));

		List<Instruction> solved = afterSolve(code, cfg, lpProblem);
		assertAllAssigned(solved);
		assertEquals(12, solved.size());
	}

	@Test
	public void testMoreSpillage() throws Exception {
		/*
		 * _do_stuff:
		 * movq %rbx, save!%rbx
		 * movq %rbp, save!%rbp
		 * movq %r12, save!%r12 
		 * movq %r13, save!%r13
		 * movq %r14, save!%r14
		 * movq %r15, save!%r15
		 * movq %rdi, a
		 * movq %rsi, b
		 * movq %rdx, c
		 * movq a, %rdi
		 * movq b, %rsi
		 * call _add
		 * movq %rax, VAR0
		 * movq VAR0, %rdi
		 * movq c, %rsi
		 * call _sub
		 * movq %rax, VAR1
		 * movq VAR1, %rax
		 * movq save!%rbx, %rbx
		 * movq save!%rbp, %rbp
		 * movq save!%r12, %r12
		 * movq save!%r13, %r13
		 * movq save!%r14, %r14
		 * movq save!%r15, %r15
		 * ret
		 */
		Variable a = new Variable("a"), b = new Variable("b"), c = new Variable("c"), var0 = new Variable("VAR0"), var1 = new Variable("VAR1"), save_rbx = new Variable("save!%rbx"), save_rbp = new Variable("save!%rbp"), save_r12 = new Variable("save!%r12"), save_r13 = new Variable("save!%r13"), save_r14 = new Variable("save!%r14"), save_r15 = new Variable("save!%r15");

		List<Instruction> code = Lists.newArrayList(
				new Definition(new Label("_do_stuff"), false),
				move(Register.rbx, save_rbx),
				move(Register.rbp, save_rbp),
				move(Register.r12, save_r12),
				move(Register.r13, save_r13),
				move(Register.r14, save_r14),
				move(Register.r15, save_r15),
				move(Register.rdi, a),
				move(Register.rsi, b),
				move(Register.rdx, c),
				move(a, Register.rdi),
				move(b, Register.rsi),
				new Call(new Label("_add")),
				move(Register.rax, var0),
				move(b, Register.rdi),
				move(c, Register.rsi),
				new Call(new Label("_sub")),
				move(Register.rax, var1),
				move(var1, Register.rax),
				move(save_rbx, Register.rbx),
				move(save_rbp, Register.rbp),
				move(save_r12, Register.r12),
				move(save_r13, Register.r13),
				move(save_r14, Register.r14),
				move(save_r15, Register.r15),
				Compiler.ret);

		// assigns: ret->rbp, a->rdi, var0->r10
		ControlFlowGraph cfg = new ControlFlowGraph(code, new Variable());
		glp_prob lpProblem = solve(cfg, code);

		// some debug printing
		printVariableLife(lpProblem, cfg, code, c);
		printRegisterLife(lpProblem, cfg, code, Register.rsi);
		printShouldBeSwitches(lpProblem, cfg, code);

		List<Instruction> solved = afterSolve(code, cfg, lpProblem);
		assertAllAssigned(solved);
		assertEquals(10, solved.size());
	}

	private static List<Instruction> afterSolve(List<Instruction> code, ControlFlowGraph cfg, glp_prob lpProblem) {
		ParsingState state = TestUtils.newParsingState();
		state.code.clear();
		state.code.addAll(code);
		copySolutionIntoCode(lpProblem, cfg, state);
		return Lists.newArrayList(Iterables.filter(state.code, Compiler.noNoOps));
	}

	private static glp_prob solve(ControlFlowGraph cfg, List<Instruction> code) {
		glp_prob lpProblem = getLPproblem(cfg);
		glp_iocp iocp = new glp_iocp();
		glp_init_iocp(iocp);
		iocp.setPresolve(GLP_ON);
		assertEquals(0, glp_intopt(lpProblem, iocp));
		return lpProblem;
	}
}
