# Dynamic Optimisation: the Next Logical Step? #

[Dynamic Optimisation](http://www.ibm.com/developerworks/library/j-jtp12214) covers a set of techniques used to optimise a piece of code at runtime based on the dynamic state and path through the code so far. The canonical example is optimising a loop: while statically-optimised version unrolls several iterations of the loop, the dynamically-optimised version has realised that the loop's predicate can never be true and so removes the loop entirely. Dynamic optimisation is most commonly applied when compiling a higher-level language into machine code, but what if we applied the technique at a higher level of abstraction?

One of my favourite interview questions is 'what implementation of data structure x would fit this use case?'. Maybe you're concentrating on the higher-level guarantees a data structure provides (e.g. uniqueness or ordering), or maybe you're prioritising time-to-market over application performance. If you could specify what implementation of a data structure to use for a given pattern of data access, or a given availability of system resources you could use a data structure that optimises its implementation over its lifetime, trading off predictability and book-keeping overhead to raise the lower bound on performance. 

I've written a proof-of-concept to highlight some of the interesting problems this data structure faces. It's a implementation of the _java.util.List_ interface that decides whether to store its data in a _LinkedList_ or an _ArrayList_. As you probably know, they perform equally for appends (although an _ArrayList_ is amortised _O(1)_ while a _LinkedList_ is _O(1)_ every time), but an _ArrayList_ gives _O(1)_ random access but _O(n)_ prepending while a _LinkedList_ gives _O(n)_ random access and _O(1)_ prepending.  

We'll start with the wrapper class, that delegates to the current backing implementation. I'll use Google's [Guava](http://code.google.com/p/guava-libraries) to reduce the boilerplate. You could write a slower but more generic implementation with the _java.lang.reflect_'s [proxying mechanism](http://docs.oracle.com/javase/1.3/docs/api/java/lang/reflect/Proxy.html), and could probably do this in a single line of a dynamic language.

~~~~
@Dynamic List Class@ +=
private static final class DynamicList<E> extends ForwardingList<E> {
  private List<E> implementation = new ArrayList<E>();
  protected List<E> delegate() {
    return implementation;
  }
  @Book keeping@
  @Other Methods@
}
~~~~

We'll need to gather data to determine the access patterns. All accesses (additions, replacements, deletions and queries) fall into two categories: accesses at the ends of the list (index 0 and the last index) and other "random" accesses. We'll do this by (tediously) instrumenting the interface's method calls:

~~~~
@Other Methods@ +=
public boolean add(E arg0) {
  endAccess();
  return super.add(arg0);
}
public void add(int index, E element) {
  accessAt(index);
  super.add(index, element);
}
public E set(int index, E element) {
  accessAt(index);
  return super.set(index, element);
}
public E get(int index) {
  accessAt(index);
  return super.get(index);
}
public E remove(int index) {
  accessAt(index);
  return super.remove(index);
}    
~~~~

Bulk accesses are marginally more interesting; we'll consider then as one access each (not _n_) as at an implementation level they only require an _O(1)_ reorganisation of pointers or a )amortised) single call to _System.arrayCopy_ (which is really _O(n)_ - it'd be interesting to see what effects the two cost-counting methods had on the structure's behaviour).  

~~~~
@Other Methods@ +=
public boolean addAll(Collection<? extends E> collection) {
  endAccess();
  return super.addAll(collection);
}
public boolean addAll(int index, Collection<? extends E> elements) {
  accessAt(index);
  return super.addAll(index, elements);
}
public List<E> subList(int fromIndex, int toIndex) {
  accessAt(fromIndex);
  return standardSubList(fromIndex, toIndex);
}
~~~~

For the actual book-keeping, we'll just have _int_ counters, and will ignore the effects of overflow. Industrial-strength solutions may want to scale the numbers down (or subtract the same constant from each of them) if any one gets too large:

~~~~
@Book keeping@ +=
private int endAccess = 0;
private int randomAccess = 0;
private void endAccess() {
  ++endAccess;
  evaluateImpl();
}
private void accessAt(int index) {
  if (index == 0 || index == size()) {
    ++endAccess;
  } else {
    ++randomAccess;
  }
  evaluateImpl();
}
~~~~

Now to the first really interesting problem. The _evaluateImpl_ method might be quite expensive (especially if it decides we need to change the implementation we're using), and once the pattern of access behaviour is obvious we don't really need the evaluation overhead when we're not going to do anything with it. Also, if the access pattern is right on the border of the trade-off between implementations we don't want to end up changing implementations backwards and forwards on every access. We'll therefore want some degree of [hysterisis](http://en.wikipedia.org/wiki/Hysteresis) - I'll go for an exponential back-off (so only consider our choice of implementation on the 1st, 2nd, 4th, 8th, 16th, &c. access). This means if the access behaviour changes later in the list's lifetime we'll only react to it very slowly. It's also likely that the pattern of accesses just after the list is created won't be representative - consider a read-only list, where the first _n_ accesses are insertions at the end (so a _LinkedList_ would be ideal) but all accesses after that are random. I'll therefore make the exponential backoff start at 32 - that is the first evaluation of the implementation's suitability will be after 32 accesses:

~~~~
@Book keeping@ +=
private int nextEvaluation = 32;
private void evaluateImpl() {
  if (randomAccess + endAccess == nextEvaluation) {
    @Evaluate Suitability@
    nextEvaluation = Math.max(nextEvaluation << 1, 0x7FFFFFFF); // don't overflow
  }
}
~~~~

And now for the key algorithm - what implementation best fits the metrics we've gathered? I'll do a trivial implementation in-line; you could probably abstract the logic behind a strategy pattern to keep it tidier. I've tried to choose numbers that give sensible results, so if the ratio of random accesses to end accesses is greater than 1:4 I believe an _ArrayList_ will probably perform better. 

~~~~
@Evaluate Suitability@ +=
if(randomAccess > endAccess / 4) {
  useArrayList();
} else {
  useLinkedList();
}
~~~~

Writing meaningful micro-benchmarks in any statically-compiled language is very hard, and in Java it's virtually impossible. Despite this, I wrote a loop that first inserts 50,000 random numbers into a list at a random index, then inserts 50,000 (different) random numbers at index 0 of a new list - to get a rough idea about whether my implementation-choosing algorithm above "works". The (meaningless) results are below, with total loop times in milliseconds:

| Test                      | ArrayList | LinkedList | DynamicList |
|:--------------------------|----------:|-----------:|------------:|
| Inserting at random index | 613       | 3,245      | 648         |
| Inserting at index 0      | 1,126     | 11         | 22          |

I (rather unethically) adjusted the ratio in my algorithm until the _DynamicList_ picked the right implementation in each benchmark - yet I still think this shows the idea of a dynamically-chosen data structure implementation is workable.

Back to the code - the switches avoid unnecessary work:

~~~~
@Other Methods@ +=
private void useLinkedList() {
  if (implementation instanceof LinkedList) return;
  implementation = new LinkedList<E>(implementation);
  @Changed Implementation@
}
~~~~

As evaluating an _ArrayList_ happens relatively infrequently you can do a bit of maintenance. I'll compact the backing array down to remove any unused trailing nulls, which should (marginally) reduce the memory footprint of my process.

~~~~
@Other Methods@ +=
private void useArrayList() {
  if (implementation instanceof ArrayList) {
    ((ArrayList<E>)implementation).trimToSize();            
    return;
  }
  implementation = new ArrayList<E>(implementation);
  @Changed Implementation@
}
~~~~

The next, more Java-specific, implementation detail is how do you deal with _Iterator_s (or more generically, long-lived mutable objects that have pointers into the _DynamicList_'s internal structures. I don't want the list itself to keep track of all the objects that have pointers into it as this makes the code less efficient (as every time you change implementation you have to update these pointers - even if these pointers are never going to be dereferenced again) and could lead to memory leaks (as there are hard-to-debug circular references that could even fool a garbage-collector). Instead I'll make all the references one-way, from the _Iterator_s to the _DynamicList_. Before accessing pointers into the _DynamicList_'s internal state they will check if the _DynamicList_ has changed implementation since the last time they checked: if so their pointers are invalid so they'll have to be refreshed (in this case, by asking the _DynamicList_ for new ones).

~~~~
@Changed Implementation@ += ++switches;
@Book keeping@ += private int switches = 0;
@Other Methods@ +=
public ListIterator<E> listIterator(int index) {
  accessAt(index);
  return new DynamicListIterator(implementation.listIterator(index), switches);
}
private class DynamicListIterator extends ForwardingListIterator<E> {
  private ListIterator<E> delegate;
  private int age; // the number of switches the last time we looked at the DynamicList
  DynamicListIterator(ListIterator<E> delegate, int age) {
    this.delegate = delegate; this.age = age;
  }
  protected ListIterator<E> delegate() {
    if (age == switches) { // no changes
      return delegate;
    } else { // the implementation's changed!
      age = switches;
      return delegate = implementation.listIterator(delegate.nextIndex());
    }
  }
}   
~~~~

I've used an integer counter to keep track of the "age" of the _DynamicList_, rather than a real timestamp of the last implementation switch. While this has a very unlikely overflow bug (if the implementation switches so many times that the counter wraps round and comes back to the number it started at - the _Iterator_ won't notice its pointers are now invalid), it avoids system calls and is more deterministic (useful when debugging application code using this list). 

We'll hide all the implementation details behind a static factory method, just in case we want to change the constructor later (e.g. to pass in hints about the initial size of the backing array of the _ArrayList_ implementation).

~~~~
@com/blogspot/remisthoughts/dynamicoptimisation/DynamicDataStructures.java:*@ +=
package com.blogspot.remisthoughts.dynamicoptimisation;
@Imports@
public final class DynamicDataStructures {
    @Dynamic List Class@
    public static @Nonnull <E> List<E> newList() {
        return new DynamicList<E>();
    }
}
~~~~

And we'll finish off with all the Java boilerplate needed to make this work:

~~~~
@Imports@ +=
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import javax.annotation.Nonnull;
import com.google.common.collect.ForwardingList;
import com.google.common.collect.ForwardingListIterator;
@Other Methods@ +=
public ListIterator<E> listIterator() {
  return listIterator(0);
}
public Iterator<E> iterator() {
  return listIterator(0);
}
~~~~

Unit testing this code will be quite difficult as there are a lot of edge cases - ideally I'd use Google's [Collections API compatibility test framework](http://code.google.com/p/google-collections/source/browse/trunk/testfw/com/google/common/collect/testing/ListTestSuiteBuilder.java) but there doesn't seem to be an easily-accessible (i.e. in the central Maven repository) compiled version of the code. 

In the meantime, you can find this code in my [Github repo](https://github.com/remis-thoughts/blog/tree/master/dynamic-optimisation)...
