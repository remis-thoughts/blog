package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.TestUtils.assertEqualStackReadsAndWrites;
import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.util.List;

import org.antlr.runtime.ANTLRInputStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.tree.Tree;
import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Label;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ProgramState;
import com.google.common.base.Charsets;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public class PreRegisterAllocationTest {
	private static Tree code(String code) throws Exception {
		return new UnsignedParser(new CommonTokenStream(new UnsignedLexer(
				new ANTLRInputStream(new ByteArrayInputStream(
						code.getBytes(Charsets.UTF_8)), "UTF-8")))).eval().tree;
	}

	@Test
	public void testGlobalInitialisationOptimisation() throws Exception {
		Tree ast = Compiler.get(code("@ABC = 23;"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		assertEquals(new Immediate(23), program.globals.get(new Label("ABC")));
	}

	@Test
	public void testGlobalInitialisationWITHOUTOptimisation() throws Exception {
		Tree ast = Compiler.get(code("@ABC = +(1, 1);"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		assertEquals(new Immediate(0), program.globals.get(new Label("ABC")));
	}

	@Test
	public void testSpillToStackAcrossFnCall() throws Exception {
		Tree ast = Compiler.get(
				code("fn main(a) { @a = +(3, 2);ret = @(a);free(a);return ret;}"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		List<Instruction> code = Lists.newArrayList(Iterables.filter(
				program.text.get(0), Compiler.noNoOps));
		assertEqualStackReadsAndWrites(code);
	}

	@Test
	public void testAnotherSpillToStackAcrossFnCall() throws Exception {
		Tree ast = Compiler.get(
				code("fn do_stuff(a, b, c) {return sub(add(a, b), c);}"), 0);
		ProgramState program = new ProgramState();
		Compiler.parseDefinition(ast, program);
		List<Instruction> code = Lists.newArrayList(Iterables.filter(
				program.text.get(0), Compiler.noNoOps));
		assertEqualStackReadsAndWrites(code);
	}

}
