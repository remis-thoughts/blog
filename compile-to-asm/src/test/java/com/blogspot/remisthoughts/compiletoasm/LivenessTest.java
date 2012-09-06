package com.blogspot.remisthoughts.compiletoasm;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.ControlFlowGraph;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Liveness;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;
import com.google.common.collect.Lists;

public class LivenessTest {
	@Test
	public void testLiveness() throws Exception {
		Variable[] vars = {new Variable("a"), new Variable("b")};
		List<Instruction> code = Lists.newArrayList(
				Op.movq.with(Register.rax, vars[0]),
				Op.movq.with(Register.rbx, vars[1]),
				Op.addq.with(vars[0], vars[1]),
				Op.movq.with(vars[1], Register.rax),
				Compiler.ret);

		Liveness liveness = Compiler.calculateLiveness(vars, code, new ControlFlowGraph(vars, code));
		assertNotNull(liveness);

		// vars[0]: "a"
		assertFalse(liveness.isLiveAt(0, 0));
		assertTrue(liveness.isLiveAt(0, 2));
		assertTrue(liveness.isLiveAt(0, 3));
		assertFalse(liveness.isLiveAt(0, 4));
		assertFalse(liveness.isLiveAt(0, 5));
		assertFalse(liveness.isLiveAt(0, 1));

		// vars[1]: "b"
		assertFalse(liveness.isLiveAt(1, 0));
		assertFalse(liveness.isLiveAt(1, 2));
		assertTrue(liveness.isLiveAt(1, 3));
		assertTrue(liveness.isLiveAt(1, 4));
		assertFalse(liveness.isLiveAt(1, 5));
		assertFalse(liveness.isLiveAt(1, 1));
	}
}
