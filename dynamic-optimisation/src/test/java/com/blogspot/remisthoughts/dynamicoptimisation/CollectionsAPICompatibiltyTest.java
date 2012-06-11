package com.blogspot.remisthoughts.dynamicoptimisation;

import java.util.Arrays;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestCase;

import com.google.common.collect.testing.ListTestSuiteBuilder;
import com.google.common.collect.testing.TestListGenerator;
import com.google.common.collect.testing.TestStringListGenerator;
import com.google.common.collect.testing.features.CollectionFeature;
import com.google.common.collect.testing.features.CollectionSize;
import com.google.common.collect.testing.features.Feature;
import com.google.common.collect.testing.features.ListFeature;

public class CollectionsAPICompatibiltyTest extends TestCase {
	private static final TestListGenerator<String> gen = new TestStringListGenerator() {
		@Override
		protected List<String> create(String[] elements) {
			List<String> ret = DynamicDataStructures.newList();
			ret.addAll(Arrays.asList(elements));
			return ret;
		}
	};
	private static final Feature<?>[] features = new Feature[]{ListFeature.GENERAL_PURPOSE, CollectionFeature.SERIALIZABLE, CollectionFeature.ALLOWS_NULL_VALUES, CollectionFeature.FAILS_FAST_ON_CONCURRENT_MODIFICATION, CollectionSize.ANY};

	public static Test suite() {
		return ListTestSuiteBuilder.using(gen).named("DynamicList").withFeatures(features).createTestSuite();
	}
}
