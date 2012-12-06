package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.move;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.Condition.ae;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.blogspot.remisthoughts.compiletoasm.Compiler.AtAddress;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Immediate;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Move;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Op;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;
import com.google.common.collect.Sets;

public class ObjectsTest {
    private final Variable a = new Variable("a");
    private final AtAddress atA = new AtAddress(a);
    private final Variable b = new Variable("b");
    private final Immediate zero = new Immediate(0);

    @Test
    public void testMemoryAddresses_move_imm2mem() throws Exception {
	assertFalse(move(zero, a).readsFrom().contains(a));
	assertTrue(move(zero, a).willWriteTo().contains(a));

	assertTrue(move(zero, atA).readsFrom().contains(a));
	assertFalse(move(zero, atA).willWriteTo().contains(a));
    }

    @Test
    public void testMemoryAddresses_move_var2mem() throws Exception {
	assertFalse(move(b, a).readsFrom().contains(a));
	assertTrue(move(b, a).readsFrom().contains(b));
	assertTrue(move(b, a).willWriteTo().contains(a));
	assertFalse(move(b, a).willWriteTo().contains(b));

	assertTrue(move(b, atA).readsFrom().contains(a));
	assertTrue(move(b, atA).readsFrom().contains(b));
	assertFalse(move(b, atA).willWriteTo().contains(a));
	assertFalse(move(b, atA).willWriteTo().contains(b));
    }

    @Test
    public void testMemoryAddresses_binop_var2mem() throws Exception {
	assertTrue(Op.addq.with(b, a).readsFrom().contains(a));
	assertTrue(Op.addq.with(b, a).readsFrom().contains(b));
	assertTrue(Op.addq.with(b, a).willWriteTo().contains(a));
	assertFalse(Op.addq.with(b, a).willWriteTo().contains(b));

	assertTrue(Op.addq.with(b, atA).readsFrom().contains(a));
	assertTrue(Op.addq.with(b, atA).readsFrom().contains(b));
	assertFalse(Op.addq.with(b, atA).willWriteTo().contains(a));
	assertFalse(Op.addq.with(b, atA).willWriteTo().contains(b));
    }

    @Test
    public void testMemoryAddresses_move_cond() throws Exception {
	assertFalse(new Move(b, a, ae).readsFrom().contains(a));
	assertTrue(new Move(b, a, ae).readsFrom().contains(b));
	assertFalse(new Move(b, a, ae).willWriteTo().contains(a));
	assertFalse(new Move(b, a, ae).willWriteTo().contains(b));

	assertTrue(new Move(b, atA, ae).readsFrom().contains(a));
	assertTrue(new Move(b, atA, ae).readsFrom().contains(b));
	assertFalse(new Move(b, atA, ae).willWriteTo().contains(a));
	assertFalse(new Move(b, atA, ae).willWriteTo().contains(b));
    }

    @Test
    public void testMemoryAddresses_binop_mem2var() throws Exception {
	assertTrue(Op.addq.with(a, b).readsFrom().contains(a));
	assertTrue(Op.addq.with(a, b).readsFrom().contains(b));
	assertFalse(Op.addq.with(a, b).willWriteTo().contains(a));
	assertTrue(Op.addq.with(a, b).willWriteTo().contains(b));

	assertTrue(Op.addq.with(atA, b).readsFrom().contains(a));
	assertTrue(Op.addq.with(atA, b).readsFrom().contains(b));
	assertFalse(Op.addq.with(atA, b).willWriteTo().contains(a));
	assertTrue(Op.addq.with(atA, b).willWriteTo().contains(b));
    }

    @Test
    public void testMemoryAddresses_move_mem2var() throws Exception {
	assertTrue(move(a, b).readsFrom().contains(a));
	assertFalse(move(a, b).readsFrom().contains(b));
	assertFalse(move(a, b).willWriteTo().contains(a));
	assertTrue(move(a, b).willWriteTo().contains(b));

	assertTrue(move(atA, b).readsFrom().contains(a));
	assertFalse(move(atA, b).readsFrom().contains(b));
	assertFalse(move(atA, b).willWriteTo().contains(a));
	assertTrue(move(atA, b).willWriteTo().contains(b));
    }

    @Test
    public void testUses() throws Exception {
	assertEquals(Sets.newHashSet(a, b), move(a, b).uses(Variable.class));
	assertEquals(Sets.newHashSet(), move(a, b).uses(Register.class));

	assertEquals(Sets.newHashSet(a, b), move(atA, b).uses(Variable.class));
	assertEquals(Sets.newHashSet(), move(atA, b).uses(Register.class));
    }

    @Test
    public void testEquality() throws Exception {
	assertEquals(a, new Variable("a"));
	assertEquals(a.hashCode(), new Variable("a").hashCode());
	assertFalse(a.equals(atA));
    }
}
