package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.move;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.ret;
import static com.google.common.collect.Iterables.filter;
import static com.google.common.collect.Iterables.isEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.Definition;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Label;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;
import com.google.common.base.Joiner;
import com.google.common.collect.Lists;

public class RegisterAllocationTest {

	private static List<Instruction> assertAllocation(List<Instruction> code) {
		code = Compiler.registerAllocation(code);
		code = Lists.newArrayList(filter(code, Compiler.noNoOps));
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
}
