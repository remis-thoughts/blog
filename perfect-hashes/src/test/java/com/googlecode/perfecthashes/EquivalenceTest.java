package com.googlecode.perfecthashes;

import org.junit.Assert;
import org.junit.Test;

public class EquivalenceTest {
	private static final Object objectWithHashCode(final int hc) {
		return new Object() {
			@Override
			public int hashCode() {
				return hc;
			}
		};
	}

	@Test
	public void testOverflow() throws Exception {
		int intMax = Integer.MAX_VALUE;
		Object o = objectWithHashCode(0);
		int n = Integer.MAX_VALUE;
		// these values will give h1 and h2 of MAX_VALUE

		int[] ret = PerfectHashes.getTwoHashes(o, intMax, intMax, n);

		Assert.assertEquals(2, ret.length);
		Assert.assertTrue(ret[0] >= 0);
		Assert.assertTrue(ret[1] >= 0);
		Assert.assertTrue(ret[0] < n);
		Assert.assertTrue(ret[1] < n);
	}
}
