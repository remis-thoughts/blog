package com.blogspot.remisthoughts.compiletoasm;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.antlr.runtime.CommonToken;
import org.antlr.runtime.tree.CommonTree;

import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Move;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ParsingState;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ProgramState;
import com.blogspot.remisthoughts.compiletoasm.Compiler.StaticStackVar;
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

	public static void assertEqualStackReadsAndWrites(List<Instruction> code) {
		int stackWrites = stackWrites(code);
		int stackReads = stackReads(code);
		if (stackReads != stackWrites) {
			throw new AssertionError(String.format("%d reads but %d writes - should be equal", stackReads, stackWrites));
		}
	}

	private static int stackReads(List<Instruction> code) {
		int ret = 0;
		for (Instruction i : code) {
			if (i instanceof Move && ((Move) i).from instanceof StaticStackVar) {
				++ret;
			}
		}
		return ret;
	}

	private static int stackWrites(List<Instruction> code) {
		int ret = 0;
		for (Instruction i : code) {
			if (i instanceof Move && ((Move) i).to instanceof StaticStackVar) {
				++ret;
			}
		}
		return ret;
	}
}
