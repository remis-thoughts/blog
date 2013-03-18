package com.blogspot.remisthoughts.compiletoasm;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.antlr.runtime.CommonToken;
import org.antlr.runtime.tree.CommonTree;

import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ParsingState;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ProgramState;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;

public class TestUtils {
	public static ParsingState newParsingState() {
		CommonTree tree = new CommonTree(new CommonToken(UnsignedParser.Definition));
		tree.addChild(new CommonTree(new CommonToken(UnsignedParser.Name)));
		tree.addChild(new CommonTree(new CommonToken(UnsignedParser.Parameters)));
		tree.addChild(new CommonTree(new CommonToken(UnsignedParser.Body)));
		return new ParsingState(tree, new ProgramState());
	}

	public static void assertAllAssigned(List<Instruction> code) {
		for (Instruction i : code) {
			assertEquals(0, i.uses(Variable.class).size());
		}
	}
}
