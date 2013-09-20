package com.blogspot.remisthoughts.compiletoasm;

import static com.google.common.collect.Iterables.isEmpty;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.util.List;

import org.antlr.runtime.ANTLRInputStream;
import org.antlr.runtime.CommonToken;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;
import org.apache.commons.lang3.StringUtils;

import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ParsingState;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ProgramState;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;
import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.io.ByteStreams;
import com.google.common.io.Closeables;
import com.google.common.io.Files;

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
			assertTrue("expected '" + i + "' to have no variables", isEmpty(i.uses(Variable.class)));
		}
	}

	private static final String GCC_CMD = "gcc -m64 -Wall -o %s %s";

	public static File compile(File asmFile) throws Exception {
		File binaryFile = new File(asmFile.getParent(), StringUtils.substringBeforeLast(asmFile.getName(), "."));
		Process gcc = Runtime.getRuntime().exec(String.format(GCC_CMD, binaryFile.getAbsolutePath(), asmFile.getAbsolutePath()));
		if (gcc.waitFor() != 0) {
			System.out.println(Files.toString(asmFile, Charsets.UTF_8));
			print(System.out, gcc.getInputStream());
			print(System.err, gcc.getErrorStream());
			fail("gcc must compile " + asmFile.getName() + " successfully");
		}
		System.out.println(asmFile.getAbsolutePath());
		System.out.println(Files.toString(asmFile, Charsets.UTF_8));
		return binaryFile;
	}

	public static String print(PrintStream where, InputStream what) throws IOException {
		String ret = new String(ByteStreams.toByteArray(what), Charsets.UTF_8);
		where.println(ret);
		return ret;
	}

	public static List<Instruction> getCode(ProgramState program) {
		return Lists.newArrayList(Iterables.filter(
				program.text.get(0), Compiler.noNoOps));
	}

	public static Tree code(String code) throws Exception {
		return new UnsignedParser(new CommonTokenStream(new UnsignedLexer(
				new ANTLRInputStream(new ByteArrayInputStream(
						code.getBytes(Charsets.UTF_8)), "UTF-8")))).eval().tree;
	}

	public static File compile(String code) throws IOException {
		File asmFile = File.createTempFile("test", ".s");

		// make asm
		InputStream in = new ByteArrayInputStream(code.getBytes(Charsets.UTF_8));
		Writer out = new OutputStreamWriter(new FileOutputStream(asmFile), Charsets.UTF_8);
		try {
			Compiler.compile(in, out);
		} catch (Exception e) {
			fail(Strings.isNullOrEmpty(e.getMessage()) ? e.toString() : e.getMessage());
		} finally {
			Closeables.closeQuietly(out); // flushes too
		}
		return asmFile;
	}
}
