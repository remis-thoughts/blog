package com.blogspot.remisthoughts.dynamicoptimisation;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import org.junit.Test;

/**
 * Check that the {@link DynamicDataStructures} fulfill their specification -
 * that is they are better than choosing the wrong implementation, even if they
 * are not as good as just choosing the right implementation.
 */
public class PerformanceTest {
	private static final int NUM_ELEMENTS = 50000;
	private final Random random = new Random(17);

	private long timeRandomAccesses(Factory f) {
		List<Integer> l = f.newList();
		long now = System.nanoTime();
		for (int i = 0; i < NUM_ELEMENTS; ++i) {
			l.add(random.nextInt(i + 1), random.nextInt());
		}
		return (System.nanoTime() - now) / 1000000;
	}

	private long timeFrontAccesses(Factory f) {
		List<Integer> l = f.newList();
		long now = System.nanoTime();
		for (int i = 0; i < NUM_ELEMENTS; ++i) {
			l.add(0, random.nextInt());
		}
		return (System.nanoTime() - now) / 1000000;
	}

	private enum Factory {
		ARRAY {
			@Override
			public List<Integer> newList() {
				return new ArrayList<Integer>(NUM_ELEMENTS);
			}
		},
		LINKED {
			@Override
			public List<Integer> newList() {
				return new LinkedList<Integer>();
			}
		},
		DYNAMIC {
			@Override
			public List<Integer> newList() {
				return DynamicDataStructures.newList();
			}
		};

		public abstract List<Integer> newList();
	}

	@Test
	public void testRandomAccess() throws Exception {
		long linked = timeRandomAccesses(Factory.LINKED);
		long dynamic = timeRandomAccesses(Factory.DYNAMIC);
		long array = timeRandomAccesses(Factory.ARRAY);
		System.out.println(String.format("RANDOM array: %,d ms, dynamic: %,d ms, linked: %,d ms", array, dynamic, linked));
		assertTrue(dynamic < linked);
	}

	@Test
	public void testFrontAccess() throws Exception {
		long linked = timeFrontAccesses(Factory.LINKED);
		long array = timeFrontAccesses(Factory.ARRAY);
		long dynamic = timeFrontAccesses(Factory.DYNAMIC);
		System.out.println(String.format("FRONT linked: %,d ms, dynamic: %,d ms, array: %,d ms", linked, dynamic, array));
		assertTrue(dynamic < array);
	}
}
