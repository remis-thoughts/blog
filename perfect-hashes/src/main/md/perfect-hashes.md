# Perfect Hashes in Java #

Given a set of _m_ keys, a minimal perfect hash function maps each key to an integer _0_ to _m-1_, and (most importantly) each key maps to a different integer. This means you can use the "perfect hash" number as a index into an array (i.e. use it as a hashmap) for guaranteed _O(1)_ insertions & lookups. I'm going explain the [BMZ](http://cmph.sourceforge.net/bmz.html) algorithm, roughly following the author's [C implmentation](http://cmph.git.sourceforge.net/git/gitweb.cgi?p=cmph/cmph;a=blob;f=src/bmz.c;h=eb3190e398316d3749181e2ba40daa4f54ee2287;hb=HEAD) as it creates perfect hashes in _O(m)_ space and time. I'll end up with an implementation of Google Guava's [Equivalence](http://docs.guava-libraries.googlecode.com/git/javadoc/com/google/common/base/Equivalence.html) as then you can use [wrappers](http://docs.guava-libraries.googlecode.com/git/javadoc/com/google/common/base/Equivalence.Wrapper.html) and standard Java HashMaps to create an efficient Collection with a minimum of wheel-reinventing. 

But first I'll start with a simple example. Working in Java is useful as we can re-use our key Objects' hashCode methods to do most of the work. However, it's unlikely that the numbers that hashCode returns are "perfect" - so we'll have to modify them deterministically. I'll use an idea I got from the [Jenkins hash](http://www.burtleburtle.net/bob/hash/doobs.html) algorithm - basically choose a seed integer and mix that with the hashCodes of the keys. As we want the resulting hashCode to lie between _0_ and _m-1_ we'll just do mod-_m_ on the result after mixing in the seed - so then now we just have to worry about choosing a seed that makes each object map to a different number. 

~~~~
@First Draft@ += 
private static final class MixSeed<E> extends Equivalence<E> {
    private final int seed;
    private final int m;
    MixSeed(int seed, int m) { this.seed = seed; this.m = m; }
    protected boolean doEquivalent(E a, E b) {
        return a.equals(b);
    }
    protected int doHash(E t) {
        return (t.hashCode() ^ seed) % m;
    }
}
~~~~

The first - draft approach is simply to guess a seed; if the resulting hashCodes are perfect, then return an Equivalence that uses that seed, but if not try again. We don't want to keep looping forever, so fix the number of tries and fail if no perfect hash is found. 

~~~~
@First Draft@ += 
public static <E> Equivalence<E> create(Collection<? extends E> keys) {
    Random r = new Random(17); // fixed seed, so deterministic
    for (int tries = 0; tries < 1000; ++tries) {
        int seed = r.nextInt();
        SortedSet<Integer> perfectHashes = Sets.newTreeSet();
        for (E key : keys) {
            perfectHashes.add((key.hashCode() ^ seed) % keys.size());
        }
        if (perfectHashes.first() == 0 && perfectHashes.last() == keys.size() - 1) {
            return new MixSeed<E>(seed, keys.size());
        }
   }
   @Give Up@
}
~~~~

This is clearly not very likely to succeed. To work out the exact probability of an iteration finding a perfect hash, we'll assume the hashCode mixed with the seed is uniformly distributed between _0_ and _m-1_. The first key can be mapped to any of the _m_ integers in this range, the second to any of the _m-1_ remaining integers, the third to the _m-2_ remaining integers, &c., and the probablity of this happening is _m/m * (m-1)/m * (m-2)/m * ... * 1/m_, which is _m!/m^m_ - so not very likely!

The BMZ algorithm takes a pretty interesting approach. To build the perfect hash in _O(m)_ time we can only store an _O(m)_ amount of state. We want to make the constant as big as possible (which uses a lot of memory - not ideal), so we could either store really big state objects, or make several queries smaller state objects (which BMZ does). The problem them becomes: (1) how do you work out what queries to make, and more importantly (2) how do you build up the state such that each key makes result in a different hash number. 

BMZ queries the state twice to get the data it needs to return the hash number, and solves the first step by a logical extension of the first draft above: instead of having one seed, have two! The Equivalence below takes the shared state _g_ (an array whose length is *not* _m_), queries it twice with the two different seeds, and combines them by simply summing the two states it finds. I've made the Equivalence Serializable so once you've done the hard work of generating it you can persist it somewhere and load it in other applications. 

~~~~
@Returned Equivalence@ += 
private static final class BMZ<E> extends Equivalence<E> implements Serializable {
    private final int seed1;
    private final int seed2;
    private final int[] g;
    BMZ(int seed1, int seed2, int[] g) { 
        this.seed1 = seed1; this.seed2 = seed2; this.g = g;
    }
    protected boolean doEquivalent(E a, E b) {
        return a.equals(b);
    }
    protected int doHash(E t) {
        int[] hashes = getTwoHashes(t, seed1, seed2, g.length); 
        return g[hashes[0]] + g[hashes[1]];
    }
}
/** we'll use this elsewhere, so let's extract this logic into its own method */
@VisibleForTesting static int[] getTwoHashes(Object t, int seed1, int seed2, int n) {
    int hc = t.hashCode(); // don't call twice - premature optimization?
    int h1 = (hc ^ seed1) % n;
    int h2 = (hc ^ seed2) % n;
    if(h1 < 0) { h1 += n; } // Java modulus gives numbers -n < h1 < n...
    if(h2 < 0) { h2 += n; } // ...but we want positive numbers to use as indices
    if(h1 == h2) { h2 = (h2 + 1) % n; } // h1 == h2 violates some assumptions (see later) - this is a quick fix!
    return new int[]{h1, h2};
}
~~~~

That was the easy part - so how do we know what to put in _g_? The BMZ algorithm centres around treating this state as a graph. Each key is mapped to an edge (so that's it uses two queries - one for the vertex at each end) and each vertex has an integer attached to it. The vertices are numbered from _0_ to _n_ (I'll use the same letters as [the paper](http://cmph.sourceforge.net/bmz.html) to make it easier to read this side-by-side), and the integer attached to each vertex _v_ is stored in the _g_ array at index _v_. This means that the lookup operation in the Equivalence above adds the two numbers attached to vertices at either end of the edge that corresponds to the key.

So how should we choose how big _n_ is? The answer again parallels the "First Draft" solution: we relax the problem slightly, and say that we only require a solution (i.e. a perfect hash Equivalence) with a reasonable probability. As above, we make several guesses, and fail if none of them reach an answer - and the relaxed problem means we can choose an _n_ that is reasonable likely to give us a solution (much easier than working out an exact answer); the paper suggests this should be _1.15m_.

~~~~
@BMZ Method@ += 
public static <E> Equivalence<E> createBMZ(
    Collection<? extends E> keys,
    int maxTries /*100 in c implementation*/, 
    double c /*1.15 suggested*/) 
{
    Random r = new Random(17); // fixed seed, so deterministic
    for (int tries = 0; tries < maxTries; ++tries) {
        int seed1 = r.nextInt();
        int seed2 = r.nextInt();
        int[] g = new int[(int)Math.ceil(c * keys.size())];
        @Put Numbers in g@
   }
   @Give Up@
}
~~~~

Now we have to choose what number to give each vertex so that the edges match to the perfect hash codes of the keys. It'll help if we break this problem down. In the following situations, _a_, _b_, _c_ and _d_ are vertices and the edges are numbered in square brackets (how we choose which number gets assigned to which edge comes later).

1. <pre>a</pre> a single vertex can be trivially assigned zero
2. <pre>a--[4]--b</pre> if either _a_ or _b_ are known, then the other is chosen so that _a+b=4_. If neither _a_ nor _b_ have been assigned then we can choose any pair of integers that sum to 4
3. <pre>a--[4]--b--[7]--c--[4]--d</pre> an interesting case - if we know the integer of one of the end vertices (_a_ or _d_) in this chain we can work out the other vertices by walking along the chain, at each step picking a number to make the edge just walked calculate correctly
4. <pre>a--b--c--a</pre> this is going to be the really hard case to solve - cycles or vertices of [degree](http://en.wikipedia.org/wiki/Degree_(graph_theory)) greater than two.

We'll therefore divide the vertices of the graph into two parts - one set that have to be solved the hard way (case 4 - called "critical nodes" in the paper), and others that can be solved by walking down chains or the other two simple cases. You can also see that loops in the graph (edges with both ends at the same vertex) will cause real problems - as (e.g.) if the edge needs to be an odd number, and the vertex stores an integer then we can't solve this graph. This is why the BMZ Equivalence class adds one to one of the hashes in a lookup if both hashes are the same - this turns loops into normal edges. 

We'll first need to convert the Objects passed to the graph into a set of edges (in _O(m)_ time and space - or we'll lose any big-O speedup this algorithm gives). We'll make our domain objects immutable, and not worry about all the garbage they make.

~~~~
@Graph Utilities@ += 
private static final class Edge {
     final int a; final int b;
     Edge(int[] ab) { this.a = ab[0]; this.b = ab[1]; } 
     public String toString() { return String.format("(%d,%d)", a, b); }
}
private static final class Graph {
    final List<Edge> edges;
    /** indexed by vertex, holds list of vertices that vertex is connected to */
    final List<Integer>[] adjacencyList;
    Graph(int n, int m) { 
        this.edges = new ArrayList<Edge>(m);
        this.adjacencyList = new List[n];  
    }
    /** @returns true if this edge is a duplicate */
    boolean addEdge(Edge e) {
        edges.add(e);
        if(getAdjacencyList(e.a).contains(e.b)) return true; // linear, but list should be v. small
        getAdjacencyList(e.a).add(e.b); 
        getAdjacencyList(e.b).add(e.a); 
        return false;
    }
    private List<Integer> getAdjacencyList(int forVertex) {
        List<Integer> ret = adjacencyList[forVertex];
        return ret == null ? (adjacencyList[forVertex] = new LinkedList<Integer>()) : ret;
    }
}
private static Graph toGraph(Collection<?> objects, int seed1, int seed2, int n) {
    Graph ret = new Graph(n, objects.size());
    for(Object object : objects) { if(ret.addEdge(new Edge(getTwoHashes(object, seed1, seed2, n)))) return null; }
    return ret;
}
@Put Numbers in g@ +=
Graph graph = toGraph(keys, seed1, seed2, g.length);
if(graph == null) { continue; } // some duplicates - try again with new seeds
~~~~

So how do we work out if a node is "critical" or not? We know that degree 0 and 1 nodes definitely aren't critical, so we'll start by eliminating them. This leaves us with the remaining tangle mess (or messes - the graph could be disconnected). We can then "strip off" any chains of edges (case 3 above) as we can solve them the easy way. We can find the ends of all the chains (if there are any) by looking through all the degree-one vertices, and then follow the chain towards the mess as far as it'll go, removing any vertices we cross from the critical set: 

~~~~
@Find Critical Nodes Helper@ +=
private static BitSet findCriticalNodes(Graph graph, int n) {
    // calculate node degrees...
    int[] degrees = new int[n];
    for(Edge edge : graph.edges){ ++degrees[edge.a]; ++degrees[edge.b]; };
    // ...and trim the chains...
    List<Integer> degree1 = new LinkedList<Integer>();
    for(int i=0; i<n; ++i) { if(degrees[i] == 1) degree1.add(i); }
    while(!degree1.isEmpty()){
        int v = degree1.remove(0); --degrees[v];
        if(graph.adjacencyList[v] != null) 
            for(int adjacent : graph.adjacencyList[v] ) 
                if(--degrees[adjacent] == 1 ) degree1.add(adjacent);
    }
    // ...and return a bitmap of critical vertices
    BitSet ret = new BitSet(n); // all non-critical by default - very useful!
    for(int i=0; i<n; ++i) { if(degrees[i] > 1) ret.set(i); }
    return ret;
}
@Put Numbers in g@ +=
BitSet criticalNodes = findCriticalNodes(graph, g.length);
~~~~

Now that we've classified the vertices into "critical" and (therefore) "non-critical" ones, we can start assigning integers to them. Assigning numbers to the critical vertices is essentially a [graph colouring](http://en.wikipedia.org/wiki/Graph_coloring) problem - we want to choose the integers so that adjacent nodes sum to the value of the edge (also - we haven't assigned the integers _0_ to _m-1_ to the edges yet!). We'll therefore decide what integer each edge should have as we go along - this gives us a bit more flexibility when we assign integers to vertices. As we've still not assigned numbers to the non-critical vertices we don't have to assign edge integers sequentially in this step. We can skip any edge integers that would require impossible combinations of vertex integers, and assign these leftover edge integers to the non-critical vertices later.

We'll therefore have a bitmap _ae_ that stores all the edge integers we've assigned so far. We can only assign each integer to an edge once or we won't end up with a perfect hash (remember, each edge is a key and a perfect hash assigns a different integer to each key).

~~~~
@Put Numbers in g@ +=
BitSet ae = new BitSet(g.length);
~~~~

We'll call the value we'll try to give to the next critical vertex _x_, and will start our assignment at the lowest critical vertex (this is an arbitary choice - we need to start our depth-first search somewhere). For each vertex we process, we must make sure the integer we give it (i.e. _x_) doesn't cause two edges to end up with the same integer (as each edge is a key, and two keys that hash to the same number means our hash isn't perfect). We'll therefore just keep incrementing the _x_ (in _getXThatSatifies_) until it doesn't break this invariant. However, we mustn't forget the other invariant - the hash of each key (i.e. integer assigned to each edge) must be between _0_ and _m-1_. We'll have to add a bit of validation every time we pick a new _x_; we'll check every adjacent vertex to make sure this new x doesn't cause the edge to have the same value as one of the other edges.  

~~~~
@Label Critical Nodes Helper@ += 
/** @returns false if we couldn't assign the integers */
private static boolean assignIntegersToCriticalVertices(Graph graph, int[] g, BitSet ae, BitSet criticalNodes) {
    int x = 0;
    List<Integer> toProcess = new LinkedList<Integer>(); 
    BitSet assigned = new BitSet(g.length);
    while(!assigned.equals(criticalNodes)) {
        BitSet unprocessed = ((BitSet)criticalNodes.clone()); unprocessed.andNot(assigned);
        toProcess.add(unprocessed.nextSetBit(0)); // start at the lowest unassigned critical vertex
        // assign another "tree" of vertices - not all critical ones are necessarily connected!
        x = processCriticalNodes(toProcess, graph, ae, g, x, assigned, criticalNodes);
        if(x < 0) return false; // x is overloaded as a failure signal
    }
    return true;
}
/** process a single "tree" of connected critical nodes, rooted at the vertex in toProcess */
private static int processCriticalNodes(List<Integer> toProcess, Graph graph, BitSet ae, int[] g, int x, BitSet assigned, BitSet criticalNodes) {
    while(!toProcess.isEmpty()) {
        int v = toProcess.remove(0);
        if(v < 0 || assigned.get(v)) continue; // there are no critical nodes || already done this vertex
        if(graph.adjacencyList[v] != null) {
            x = getXThatSatifies(graph.adjacencyList[v], x, ae, assigned, g);
            for(Integer adjacent : graph.adjacencyList[v]) {
                if(!assigned.get(adjacent) && criticalNodes.get(adjacent) && v!= adjacent) { 
                    // give this one an integer, & note we shouldn't have loops - except if there is one key 
                    toProcess.add(adjacent); 
                } 
                if(assigned.get(adjacent)) {  
                    int edgeXtoAdjacent = x + g[adjacent]; // if x is ok, then this edge is now taken
                    if(edgeXtoAdjacent >= graph.edges.size()) return -1; // this edge is too big! we're only assigning between 0 & m-1
                    ae.set(edgeXtoAdjacent); 
                } 
            }
        }
        g[v] = x; assigned.set(v); // assign candidate x to g
        ++x; // next v needs a new candidate x
    } 
    return x; // will use this as a candidate for other "trees" of critical vertices
}
private static int getXThatSatifies(List<Integer> adjacencyList, int x, BitSet ae, BitSet assigned, int[] g) {
    for(Integer adjacent : adjacencyList) {
        if(assigned.get(adjacent) /*only covers critical nodes*/ && ae.get(g[adjacent] + x)) { 
            // if we assign x to v, then the edge between v & and 'adjacent' will
            // be a duplicate - so our hash code won't be perfect! Try again with a new x:
            return getXThatSatifies(adjacencyList, x + 1, ae, assigned, g);
        } 
    }
    return x; // this one satisfies all edges
}
@Put Numbers in g@ +=
if(!assignIntegersToCriticalVertices(graph, g, ae, criticalNodes)) continue; // try again from the start with different seeds
~~~~

We've done the hard part - now it's all downhill from here. We've got all integers we haven't assigned to edges as zeros in the _ae_ _BitSet_, and we know that the edges between vertices in the non-critical group are just single chains (i.e case 3 above). We'll therefore do a breadth-first search of the vertices starting at the critical ones, and every time we go from a critical to a non-critical vertex or go from one non-critical vertex to another we'll assign integers to those non-critical vertices so that the edge between them is the next edge unassigned in the _ae_ set:

~~~~
@Label Non Critical Nodes Helper@ += 
private static void assignIntegersToNonCriticalVertices(Graph graph, int[] g, BitSet ae, BitSet criticalNodes) {
    List<Integer> toProcess = new LinkedList<Integer>();
    for(int v = criticalNodes.nextSetBit(0); v != -1; v = criticalNodes.nextSetBit(v+1)) { toProcess.add(v); } // load with the critical vertices
    BitSet visited = (BitSet) criticalNodes.clone();
    processNonCriticalNodes(toProcess, graph, ae, visited, g); // process the critical nodes
    // we've done everything reachable from the critical nodes - but
    // what about isolated chains?
    for(int v = visited.nextClearBit(0); v != -1 && v < g.length; v = visited.nextClearBit(v+1)) { 
        toProcess.add(v);
        processNonCriticalNodes(toProcess, graph, ae, visited, g);
    }    
}
/** process everything in the list and all vertices reachable from it */
private static void processNonCriticalNodes(List<Integer> toProcess, Graph graph, BitSet ae, BitSet visited, int[] g) {
    int nextEdge = ae.nextClearBit(0);
    while(!toProcess.isEmpty()) {
        int v = toProcess.remove(0);
        if(v < 0) continue; // there are no critical nodes
        if(graph.adjacencyList[v] != null) {
            for(int adjacent : graph.adjacencyList[v]) {
                if(!visited.get(adjacent) && v != adjacent) { // shouldn't have loops - only if one key 
                    // we must give it a value
                    g[adjacent] = nextEdge - g[v]; // i.e. g[v] + g[a] = edge as needed
                    toProcess.add(adjacent);
                    ae.set(nextEdge);
                    nextEdge = ae.nextClearBit(nextEdge + 1);                    
                }
            }
        }
        visited.set(v);
    } 
}
@Put Numbers in g@ +=
assignIntegersToNonCriticalVertices(graph, g, ae, criticalNodes); // this can't fail
~~~~

And that's it! Every vertex has a value so our graph is complete. We'll just return it, wrapped in the Equivalence we made above:

~~~~
@Put Numbers in g@ += 
return new BMZ<E>(seed1, seed2, g);
~~~~

To make this code into a useful library we'll add an public static method that chooses the hash algorithm and fills in some of the default parameters:

~~~~
@Generic Method@ +=
/** makes a perfect hash function for the given set of keys */
public static <E> Equivalence<E> create(Collection<? extends E> keys) {
    return createBMZ(keys, 100, 1.15);
}
~~~~

And here's the overall framework of the class:

~~~~
@com/googlecode/perfecthashes/PerfectHashes.java:*@ +=
package com.googlecode.perfecthashes;
@Imports@
public final class PerfectHashes {
    /*
    @First Draft@
    */
    @Returned Equivalence@
    @BMZ Method@
    @Generic Method@
    // Helpers
    @Graph Utilities@
    @Find Critical Nodes Helper@
    @Label Critical Nodes Helper@
    @Label Non Critical Nodes Helper@
}
~~~~

And some final Java boilerplate:

~~~~
@Imports@ += 
import java.io.Serializable;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Equivalence;
@Give Up@ +=
throw new IllegalStateException("giving up - perfect hashcode too hard to find!");
~~~~

And we're finished!