package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.move;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.AtAddress;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Resolvable;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;

public class ObjectsTest {
    private final Variable a = new Variable("a");
    private final Immediate zero = new Immediate(0);
    private final AtAddress atA = new AtAddress(a);

    @Test
    public void testUnwrapping1() throws Exception {

	assertTrue(move(zero, atA).uses(Resolvable.class).contains(atA));
	assertTrue(move(zero, atA).uses(AtAddress.class).contains(atA));
	assertTrue(move(zero, atA).uses(Immediate.class).contains(zero));
    }

    @Test
    public void testEquality() throws Exception {
	assertEquals(a, new Variable("a"));
	assertEquals(a.hashCode(), new Variable("a").hashCode());
	assertFalse(a.equals(atA));
    }
}
