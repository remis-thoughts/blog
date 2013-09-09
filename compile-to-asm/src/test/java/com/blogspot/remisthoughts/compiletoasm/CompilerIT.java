package com.blogspot.remisthoughts.compiletoasm;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.common.io.ByteStreams;
import com.google.common.io.Closeables;
import com.google.common.io.Files;
import com.google.common.io.Resources;

/**
 * The (expected) exit code of each program should be the integer part of the
 * file name.
 */
@RunWith(Parameterized.class)
public class CompilerIT {
	private static final String GCC_CMD = "gcc -m64 -Wall -o %s %s";

	@Parameters
	public static List<Object[]> params() {
		File folder = new File(Resources.getResource("test-code").getFile());
		List<Object[]> ret = Lists.newArrayList();
		for (File file : folder.listFiles()) {
			ret.add(new Object[] { file });
		}
		return ret;
	}

	private static File tmp = new File(System.getProperty("java.io.tmpdir"));
	private final File src;

	public CompilerIT(File src) {
		this.src = src;
	}

	@Before
	public void setup() {
		Compiler.uniqueness.set(8);
	}

	@Test
	public void testItCompiles() throws Exception {
		InputStream in = null;
		OutputStream out = null;
		try {
			String exitCode = StringUtils.substringBefore(src.getName(), ".");
			File asmFile = new File(tmp, exitCode + ".s");

			// make asm
			out = new FileOutputStream(asmFile);
			try {
				Compiler.compile(in = new FileInputStream(src), out);
			} catch (Exception e) {
				fail(Strings.isNullOrEmpty(e.getMessage()) ? e.toString() : e.getMessage());
			} finally {
				Closeables.closeQuietly(out); // flushes too
			}

			// compile asm to native binary (assumes gcc availability)
			File binaryFile = new File(tmp, exitCode);
			Process gcc = Runtime.getRuntime().exec(String.format(GCC_CMD, binaryFile.getAbsolutePath(), asmFile.getAbsolutePath()));
			if (gcc.waitFor() != 0) {
				System.out.println(Files.toString(asmFile, Charsets.UTF_8));
				print(System.out, gcc.getInputStream());
				print(System.err, gcc.getErrorStream());
				fail("gcc must compile " + src.getName() + " successfully");
			}
			System.out.println(asmFile.getAbsolutePath());
			System.out.println(Files.toString(asmFile, Charsets.UTF_8));

			// run the file and check it produces the correct exit code
			Process bin = Runtime.getRuntime().exec(binaryFile.getAbsolutePath());
			print(System.out, bin.getInputStream());
			print(System.err, bin.getErrorStream());
			if (Integer.parseInt(exitCode) != bin.waitFor()) {
				fail("must have correct exit code " + exitCode + ", not " + bin.waitFor());
			}

		} finally {
			Closeables.closeQuietly(in);
		}
	}

	private static void print(PrintStream where, InputStream what) throws IOException {
		where.println(new String(ByteStreams.toByteArray(what)));
	}
}
