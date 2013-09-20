package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.TestUtils.assertAllAssigned;
import static com.blogspot.remisthoughts.compiletoasm.TestUtils.code;
import static com.blogspot.remisthoughts.compiletoasm.TestUtils.getCode;
import static org.junit.Assert.assertEquals;

import java.util.List;

import org.antlr.runtime.tree.Tree;
import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.BinaryOp;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Label;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ProgramState;
import com.google.common.collect.Iterables;

public class PreRegisterAllocationTest {
	@Test
	public void testWhileWITHConstant() throws Exception {
		Tree ast = Compiler.get(code("while 0x1 { $a = 0x3; return $a; };"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		List<Instruction> code = getCode(program);
		assertEquals(1, Iterables.size(Iterables.filter(code, BinaryOp.class)));
		assertAllAssigned(code);
	}

	@Test
	public void testWhileNOConstant() throws Exception {
		Tree ast = Compiler.get(code("$a = 0x3; while >($a, 0x5) { $a = 0x2; };"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		List<Instruction> code = getCode(program);
		assertEquals(2, Iterables.size(Iterables.filter(code, BinaryOp.class)));
		assertAllAssigned(code);
	}

	@Test
	public void testGlobalInitialisationOptimisation() throws Exception {
		Tree ast = Compiler.get(code("@ABC = 0x17;"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		assertEquals(new Immediate(23), program.globals.get(new Label("_ABC")));
	}

	@Test
	public void testGlobalInitialisationWITHOUTOptimisation() throws Exception {
		Tree ast = Compiler.get(code("@ABC = +(0x1, 0x1);"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		assertEquals(new Immediate(0), program.globals.get(new Label("_ABC")));
	}

	@Test
	public void testSpillToStackAcrossFnCall() throws Exception {
		Tree ast = Compiler.get(
				code("fn main($a) { @a = +(0x3, 0x2); ret = @($a); free($a); return ret; };"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		List<Instruction> code = getCode(program);
		assertAllAssigned(code);
	}

	@Test
	public void testAnotherSpillToStackAcrossFnCall() throws Exception {
		Tree ast = Compiler.get(
				code("fn do_stuff($a, $b, $c) {return sub(add($a, $b), $c);};"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		List<Instruction> code = getCode(program);
		assertAllAssigned(code);
	}

}
