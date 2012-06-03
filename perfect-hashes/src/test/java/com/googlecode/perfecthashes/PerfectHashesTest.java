package com.googlecode.perfecthashes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Random;
import java.util.Set;
import java.util.SortedSet;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import com.google.common.base.Equivalence;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

@RunWith(Parameterized.class)
public class PerfectHashesTest {
	@Parameters
	public static Collection<Object[]> params() {
		Collection<Object[]> ret = Lists.newArrayList();
		for (int i = 0; i < 1000; ++i) {
			Set<String> keys = Sets.newHashSet();
			Random r = new Random(17);
			// add i keys
			for (int j = 0; j < i; ++j) {
				keys.add(randomString(r));
			}
			if (keys.size() == i) { // no duplicates
				ret.add(new Object[] { keys });
			}
		}
		return ret;
	}

	private static String randomString(Random r) {
		StringBuilder ret = new StringBuilder();
		for (int i = 0; i < 15; ++i) {
			ret.append('a' + r.nextInt(26));
		}
		return ret.toString();
	}

	private final Set<String> keys;

	public PerfectHashesTest(Set<String> keys) {
		this.keys = keys;
	}

	private static int globalFailures = 0;

	@Test
	public void testBMZ() throws Exception {
		try {
			// we don't want failure in a test, so have a huge number of
			// iterations
			// and a much bigger graph
			Equivalence<String> mph = PerfectHashes.createBMZ(keys, 10000, 2.5);

			// verify that a perfect hash was generated
			SortedSet<Integer> hashes = Sets.newTreeSet();
			for (String key : keys) {
				hashes.add(mph.hash(key));
			}
			assertEquals("hashes must all be different", keys.size(),
					hashes.size());
			if (!hashes.isEmpty()) {
				assertEquals("lowest hash is 0", 0, hashes.first().intValue());
				assertEquals("highest hash is m-1", keys.size() - 1, hashes
						.last().intValue());
			}
		} catch (IllegalStateException e) {
			assertTrue(
					"we can't guaratee all 1000 attempts converge - so this is an acceptable number of failures",
					++globalFailures < 13);
		}
	}
}
