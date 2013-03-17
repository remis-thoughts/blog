package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.move;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.Condition;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ControlFlowGraph;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Move;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;
import com.google.common.collect.Lists;

public class LivenessTest {
	@Test
	public void testcfg() throws Exception {
		Variable[] vars = { new Variable("a"), new Variable("b") };
		List<Instruction> code = Lists.newArrayList(
				move(Register.rax, vars[0]),
				move(Register.rbx, vars[1]),
				Op.addq.with(vars[0], vars[1]),
				move(vars[1], Register.rax),
				Compiler.ret);

		ControlFlowGraph cfg = new ControlFlowGraph(code, null);

		// vars[0]: "a"
		assertFalse(cfg.isLiveAt(0, 0));
		assertTrue(cfg.isLiveAt(0, 2));
		assertTrue(cfg.isLiveAt(0, 3));
		assertFalse(cfg.isLiveAt(0, 4));
		assertFalse(cfg.isLiveAt(0, 5));
		assertFalse(cfg.isLiveAt(0, 1));

		// vars[1]: "b"
		assertFalse(cfg.isLiveAt(1, 0));
		assertFalse(cfg.isLiveAt(1, 2));
		assertTrue(cfg.isLiveAt(1, 3));
		assertTrue(cfg.isLiveAt(1, 4));
		assertFalse(cfg.isLiveAt(1, 5));
		assertFalse(cfg.isLiveAt(1, 1));
	}

	/**
	 * x = 2; x = +(x, 3); return x;
	 */
	@Test
	public void testOverlapping() throws Exception {
		Variable[] vars = { new Variable("a"), new Variable("b") };
		List<Instruction> code = Lists.newArrayList(
				move(new Immediate(2), vars[0]), // mov 2, a
				move(vars[0], vars[1]), // mov a, b
				Op.addq.with(new Immediate(3), vars[1]), // add 3, b
				move(vars[1], vars[0]), // mov b, a
				move(vars[0], Register.rax), // mov a, %rax
				Compiler.ret);

		ControlFlowGraph cfg = new ControlFlowGraph(code, null);

		// vars[0]: "a"
		assertFalse(cfg.isLiveAt(0, 0));
		assertTrue(cfg.isLiveAt(0, 2));
		assertFalse(cfg.isLiveAt(0, 3));
		assertFalse(cfg.isLiveAt(0, 4));
		assertTrue(cfg.isLiveAt(0, 5));
		assertFalse(cfg.isLiveAt(0, 6));
		assertFalse(cfg.isLiveAt(0, 1));

		// vars[1]: "b"
		assertFalse(cfg.isLiveAt(1, 0));
		assertFalse(cfg.isLiveAt(1, 2));
		assertTrue(cfg.isLiveAt(1, 3));
		assertTrue(cfg.isLiveAt(1, 4));
		assertFalse(cfg.isLiveAt(1, 5));
		assertFalse(cfg.isLiveAt(1, 6));
		assertFalse(cfg.isLiveAt(1, 1));
	}

	/**
	 * x = >(2, 3)
	 */
	@Test
	public void testConditionalMoves() throws Exception {
		Variable[] vars = { new Variable("x"), new Variable("zero"), new Variable("ret") };
		List<Instruction> code = Lists.newArrayList(
				move(new Immediate(2), vars[0]), // mov 2, x
				Op.cmpq.with(new Immediate(3), vars[0]), // cmp 3, x
				move(new Immediate(1), vars[2]), // mov 1, ret
				move(new Immediate(0), vars[1]), // mov 0, zero
				new Move(vars[1], vars[2], Condition.b), // cmovb zero, ret
				move(vars[2], Register.rax),
				Compiler.ret);

		ControlFlowGraph cfg = new ControlFlowGraph(code, null);

		// vars[0]: "x"
		int x = Compiler.indexOf(cfg.variables, vars[0]);
		assertFalse(cfg.isLiveAt(x, 0));
		assertTrue(cfg.isLiveAt(x, 2));
		assertFalse(cfg.isLiveAt(x, 3));
		assertFalse(cfg.isLiveAt(x, 4));
		assertFalse(cfg.isLiveAt(x, 5));
		assertFalse(cfg.isLiveAt(x, 6));
		assertFalse(cfg.isLiveAt(x, 7));

		// vars[1]: "zero"
		int zero = Compiler.indexOf(cfg.variables, vars[1]);
		assertFalse(cfg.isLiveAt(zero, 0));
		assertFalse(cfg.isLiveAt(zero, 2));
		assertFalse(cfg.isLiveAt(zero, 3));
		assertFalse(cfg.isLiveAt(zero, 4));
		assertTrue(cfg.isLiveAt(zero, 5));
		assertFalse(cfg.isLiveAt(zero, 6));
		assertFalse(cfg.isLiveAt(zero, 7));

		// vars[2]: "ret"
		int ret = Compiler.indexOf(cfg.variables, vars[2]);
		assertFalse(cfg.isLiveAt(ret, 0));
		assertFalse(cfg.isLiveAt(ret, 2));
		assertFalse(cfg.isLiveAt(ret, 3));
		assertTrue(cfg.isLiveAt(ret, 4));
		assertTrue(cfg.isLiveAt(ret, 5));
		assertTrue(cfg.isLiveAt(ret, 6));
		assertFalse(cfg.isLiveAt(ret, 7));
	}
}
