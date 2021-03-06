package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.TestUtils.assemble;
import static com.blogspot.remisthoughts.compiletoasm.TestUtils.link;
import static com.blogspot.remisthoughts.compiletoasm.TestUtils.print;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;
import com.google.common.io.Files;
import com.google.common.io.Resources;

/**
 * The (expected) exit code of each program should be the integer part of the
 * file name.
 */
@RunWith(Parameterized.class)
public class CompilerIT {
	@Parameters
	public static List<Object[]> params() {
		File folder = new File(Resources.getResource("test-code").getFile());
		List<Object[]> ret = Lists.newArrayList();
		for (File file : folder.listFiles()) {
			if (file.getName().endsWith(".uu")) {
				ret.add(new Object[] { file });
			}
		}
		return ret;
	}

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
		String exitCode = StringUtils.substringBefore(src.getName(), ".");
		File asmFile = new File(src.getParentFile(), Files.getNameWithoutExtension(src.getName()) + ".s");

		try {
			Compiler.main(new String[] { src.getAbsolutePath() });
		} catch (Exception e) {
			fail(src.getName() + ": " + e.getMessage());
		}

		// useful to see it
		System.out.println(asmFile.getAbsolutePath());
		System.out.println(Files.toString(asmFile, Charsets.UTF_8));

		// compile asm to native binary (assumes gcc availability)
		File binaryFile = link(assemble(asmFile));

		// run the file and check it produces the correct exit code
		Process bin = Runtime.getRuntime().exec(binaryFile.getAbsolutePath());
		print(System.out, bin.getInputStream());
		print(System.err, bin.getErrorStream());
		if (Integer.parseInt(exitCode) != bin.waitFor()) {
			fail("must have correct exit code " + exitCode + ", not " + bin.waitFor());
		}
	}
}
