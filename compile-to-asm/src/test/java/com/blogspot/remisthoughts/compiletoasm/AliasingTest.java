package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.move;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.ret;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.ControlFlowGraph;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Definition;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Label;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;

public class AliasingTest {
	@Test
	public void testAliasing() throws Exception {
		Variable a = new Variable("a");
		Variable b = new Variable("b");
		Immediate one = new Immediate(1);
		Variable[] vars = {a, b};

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

		ControlFlowGraph cfg = new ControlFlowGraph(vars, code);

		assertEquals(0, cfg.aliasingVars.get(0).size());
		assertEquals(0, cfg.aliasingVars.get(1).size());
		assertEquals(0, cfg.aliasingVars.get(2).size());
		assertEquals(2, cfg.aliasingVars.get(3).size());
		assertEquals(0, cfg.aliasingVars.get(4).size());
		assertEquals(2, cfg.aliasingVars.get(5).size());
		assertEquals(0, cfg.aliasingVars.get(6).size());
		assertEquals(0, cfg.aliasingVars.get(7).size());
	}
}
