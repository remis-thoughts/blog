package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.TestUtils.compile;
import static com.blogspot.remisthoughts.compiletoasm.TestUtils.print;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

public class SymbolTest {

	@Test
	public void testExportFunction() throws Exception {
		File binary = compile(compile("fn HAT() { return 0x1; };"));
		String in = nm(binary);
		assertTrue(in.contains(" T _HAT\n"));
	}

	@Test
	public void testPrivateFunction() throws Exception {
		File binary = compile(compile("fn hat() { return 0x1; };"));
		String in = nm(binary);
		assertTrue(in.contains(" t _hat\n"));
	}

	@Test
	public void testExportDataVariable() throws Exception {
		File binary = compile(compile("@HAT = 0x2;"));
		String in = nm(binary);
		assertTrue(in.contains(" D _HAT\n"));
	}

	@Test
	public void testPrivateDataVariable() throws Exception {
		File binary = compile(compile("@hat = 0x2;"));
		String in = nm(binary);
		assertTrue(in.contains(" d _hat\n"));
	}

	@Test
	public void testExportBssVariable() throws Exception {
		File binary = compile(compile("@HAT = 0x0;"));
		String in = nm(binary);
		assertTrue(in.contains(" B _HAT\n"));
	}

	@Test
	public void testPrivateBssVariable() throws Exception {
		File binary = compile(compile("@hat = 0x0;"));
		String in = nm(binary);
		assertTrue(in.contains(" b _hat\n"));
	}

	private String nm(File binary) throws IOException, InterruptedException {
		Process bin = Runtime.getRuntime().exec("nm -U " + binary.getAbsolutePath());
		String in = print(System.out, bin.getInputStream());
		print(System.err, bin.getErrorStream());
		assertEquals(0, bin.waitFor());
		return in;
	}
}
