package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.leave;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.ret;
import static com.google.common.collect.Iterables.filter;
import static com.google.common.collect.Iterables.isEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.BinaryOp;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Definition;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Enter;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Label;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.StackVar;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Value;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

@SuppressWarnings("unchecked")
public class RegisterAllocationTest {
	private static List<Instruction> assertAllocation(List<Instruction>... codes) {
		return assertAllocation(Lists.newArrayList(Iterables.concat(codes)));
	}

	private static List<Instruction> assertAllocation(List<Instruction> code) {
		code = Compiler.registerAllocation(code);
		for (Instruction i : code) {
			assertTrue("expected '" + i + "' to have no variables", isEmpty(filter(i.getParameters(), Variable.class)));
		}
		System.out.println(Joiner.on('\n').join(code));
		return code;
	}

	private static final List<Instruction> header = Arrays.asList(new Definition(new Label("test"), true), new Enter());
	private static List<Instruction> footer(Value v) {
		return Arrays.asList(Op.movl.with(v, Register.eax), leave, ret);
	}

	private static List<Instruction> add(Variable a, Variable b, int one, int two, Variable c) {
		return Arrays.asList(
				Op.movl.with(new Immediate(one), a),
				Op.movl.with(new Immediate(two), b),
				Op.movl.with(a, c),
				Op.addl.with(b, c));
	}

	private static List<Instruction> add(Variable a, Variable b, Variable c) {
		return Arrays.asList(
				Op.movl.with(a, c),
				Op.addl.with(b, c));
	}

	/**
	 * an x86 processor emulator!
	 */
	private static void assertEvalsTo(List<Instruction> code, int expected) {
		int[] registers = new int[Register.values().length];
		int[] stack = new int[20];
		for (Instruction i : code) {
			if (!(i instanceof BinaryOp)) {
				continue;
			}
			BinaryOp b = (BinaryOp) i;
			int arg1, arg2;
			switch (b.op) {
				case addl :
					arg1 = get(b.arg1, registers, stack);
					arg2 = get(b.arg2, registers, stack);
					put(b.arg2, registers, stack, arg1 + arg2);
					break;
				case movl :
					arg1 = get(b.arg1, registers, stack);
					put(b.arg2, registers, stack, arg1);
					break;
				default :
					fail();
			}
		}
		assertEquals(expected, registers[Register.eax.ordinal()]);
	}

	private static void put(Value v, int[] registers, int[] stack, int arg) {
		if (v instanceof Register) {
			registers[((Register) v).ordinal()] = arg;
		} else if (v instanceof StackVar) {
			stack[((StackVar) v).var] = arg;
		} else {
			throw new AssertionError();
		}
	}

	private static int get(Value v, int[] registers, int[] stack) {
		if (v instanceof Register) {
			return registers[((Register) v).ordinal()];
		} else if (v instanceof Immediate) {
			return ((Immediate) v).value.intValue();
		} else if (v instanceof StackVar) {
			return stack[((StackVar) v).var];
		} else {
			throw new AssertionError();
		}
	}

	// TESTS

	@Test
	public void testNoAssignment() throws Exception {
		List<Instruction> code = assertAllocation(header, footer(new Immediate(3)));
		assertEvalsTo(code, 3);
	}

	@Test
	public void testSomeVarsNoSpilling() throws Exception {
		Variable c = new Variable("c");
		List<Instruction> code = assertAllocation(header, add(new Variable("a"), new Variable("b"), 1, 5, c), footer(c));
		assertEvalsTo(code, 6);
	}

	@Test
	public void testSpillage() throws Exception {
		Variable[] vs = new Variable[15];
		for (int i = 0; i < vs.length; i++) {
			vs[i] = new Variable(String.format("%02dv", i));
		}
		List<Instruction> code = assertAllocation(
				header,
				add(vs[0], vs[1], 0, 1, vs[8]),
				add(vs[2], vs[3], 2, 3, vs[9]),
				add(vs[4], vs[5], 4, 5, vs[10]),
				add(vs[6], vs[7], 6, 7, vs[11]),
				add(vs[8], vs[9], vs[12]),
				add(vs[10], vs[11], vs[13]),
				add(vs[12], vs[13], vs[14]),
				footer(vs[14]));
		assertEquals(15, Iterables.filter(code, Enter.class).iterator().next().numLocals);
		assertEvalsTo(code, 0 + 1 + 2 + 3 + 4 + 5 + 6 + 7);
	}
}
