package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.move;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.ret;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.AtAddress;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ControlFlowGraph;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Definition;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Label;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Parameter;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ParsingState;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;

public class AliasingTest {
	private final ParsingState state = TestUtils.newParsingState();

	@Test
	public void testAliasing() throws Exception {
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

		ControlFlowGraph cfg = new ControlFlowGraph(code);

		assertEquals(0, cfg.aliasingVars.get(0).size());
		assertEquals(0, cfg.aliasingVars.get(1).size());
		assertEquals(0, cfg.aliasingVars.get(2).size());
		assertEquals(2, cfg.aliasingVars.get(3).size()); // a, b
		assertEquals(0, cfg.aliasingVars.get(4).size());
		assertEquals(2, cfg.aliasingVars.get(5).size()); // b, a
		assertEquals(0, cfg.aliasingVars.get(6).size());
		assertEquals(0, cfg.aliasingVars.get(7).size());
	}

	@Test
	public void testAliasingWithMemory1() throws Exception {
		Variable a = new Variable("a");
		AtAddress atA = new AtAddress(a, 0);
		Variable b = new Variable("b");
		Immediate one = new Immediate(1);
		Parameter p = new Parameter(state, 0);

		/*
		 * fn one_plus_one(a) { 
		 *   @a = 1;
		 *   b = @(a);
		 *   return b;
		 * }
		 */
		List<Instruction> code = Arrays.asList(
				new Definition(new Label("one_plus_one"), false),
				move(p, a), // w:a
				move(one, atA), // r:a
				move(atA, b), // r:a, w:b
				move(b, Register.rax), // r:b
				ret);

		ControlFlowGraph cfg = new ControlFlowGraph(code);

		assertEquals(0, cfg.aliasingVars.get(0).size());
		assertEquals(0, cfg.aliasingVars.get(1).size());
		assertEquals(0, cfg.aliasingVars.get(2).size());
		assertEquals(0, cfg.aliasingVars.get(3).size());
		assertEquals(2, cfg.aliasingVars.get(4).size());
		assertEquals(0, cfg.aliasingVars.get(5).size());
		assertEquals(0, cfg.aliasingVars.get(6).size());
	}

	@Test
	public void testAliasingWithMemory2() throws Exception {
		Variable a = new Variable("a");
		AtAddress atA = new AtAddress(a, 0);
		Variable b = new Variable("b");
		Immediate one = new Immediate(1);
		Parameter p = new Parameter(state, 0);

		/*
		 * fn one_plus_one(a) { 
		 *   b = 1;
		 *   @a = b;
		 *   return @(a);
		 * }
		 */
		List<Instruction> code = Arrays.asList(
				new Definition(new Label("one_plus_one"), false),
				move(p, a), // w:a
				move(one, b), // w:b
				move(b, atA), // r:a+b
				move(atA, Register.rax), // r:a
				ret);

		ControlFlowGraph cfg = new ControlFlowGraph(code);

		assertEquals(0, cfg.aliasingVars.get(0).size());
		assertEquals(0, cfg.aliasingVars.get(1).size());
		assertEquals(0, cfg.aliasingVars.get(2).size());
		assertEquals(0, cfg.aliasingVars.get(3).size());
		assertEquals(0, cfg.aliasingVars.get(4).size());
		assertEquals(0, cfg.aliasingVars.get(5).size());
		assertEquals(0, cfg.aliasingVars.get(6).size());
	}
}
