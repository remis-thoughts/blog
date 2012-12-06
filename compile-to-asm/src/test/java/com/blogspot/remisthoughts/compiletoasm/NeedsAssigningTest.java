package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.move;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.ret;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.Call;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ControlFlowGraph;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Definition;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Label;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;

public class NeedsAssigningTest {
	@Test
	public void testNeedsAssigning() throws Exception {
		Variable a = new Variable("a");
		Variable b = new Variable("b");
		Immediate one = new Immediate(1);

		/*
		 * a = 1;
		 * a = +(a, 1);
		 * a = sqrt(a);
		 * return a;
		 */
		List<Instruction> code = Arrays.asList(
				new Definition(new Label("one_plus_one"), false),
				move(one, a),
				move(a, b),
				Op.addq.with(one, b),
				move(b, a),
				move(a, Register.rdi),
				new Call(new Label("_sqrt")),
				move(Register.rax, a),
				move(a, Register.rax),
				ret);

		ControlFlowGraph cfg = new ControlFlowGraph(code);
		assertEquals(11, cfg.numNodes);

		// a
		assertFalse(cfg.needsAssigning(0, 0, Register.rax));
		assertFalse(cfg.needsAssigning(0, 1, Register.rax));
		assertTrue(cfg.needsAssigning(0, 2, Register.rax));
		assertTrue(cfg.needsAssigning(0, 3, Register.rax));
		assertFalse(cfg.needsAssigning(0, 4, Register.rax));
		assertTrue(cfg.needsAssigning(0, 5, Register.rax));
		assertTrue(cfg.needsAssigning(0, 6, Register.rax));
		assertFalse(cfg.needsAssigning(0, 7, Register.rax)); // call: rax is not the stack & is not preserved
		assertTrue(cfg.needsAssigning(0, 8, Register.rax));
		assertTrue(cfg.needsAssigning(0, 9, Register.rax));
		assertFalse(cfg.needsAssigning(0, 10, Register.rax));

		// b
		assertFalse(cfg.needsAssigning(1, 0, Register.rax));
		assertFalse(cfg.needsAssigning(1, 1, Register.rax));
		assertFalse(cfg.needsAssigning(1, 2, Register.rax));
		assertTrue(cfg.needsAssigning(1, 3, Register.rax));
		assertTrue(cfg.needsAssigning(1, 4, Register.rax));
		assertTrue(cfg.needsAssigning(1, 5, Register.rax));
		assertFalse(cfg.needsAssigning(1, 6, Register.rax));
		assertFalse(cfg.needsAssigning(1, 7, Register.rax));
		assertFalse(cfg.needsAssigning(1, 8, Register.rax));
		assertFalse(cfg.needsAssigning(1, 9, Register.rax));
		assertFalse(cfg.needsAssigning(1, 10, Register.rax));
	}
}
