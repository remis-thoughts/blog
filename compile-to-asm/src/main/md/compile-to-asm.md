# Writing a Compiler: x86 Assembly Code Generation #

http://meri-stuff.blogspot.co.uk/2011/09/antlr-tutorial-expression-language.html#RewriteRules
http://www.antlr.org/wiki/display/~admin/2008/11/30/Example+tree+rewriting+with+patterns

~~~~
@Antlr Parse Rules@ +=
eval : ((statement SC) | fundef)+ -> ^(Root fundef* ^(Definition ^(Name Global["main"]) ^(Parameters) ^(Body statement*)));
statement : funcall | ret | assign;
expression : variable | Number | funcall | intrinsic;
variable : Local | Global;
ret : Return expression -> ^(Return expression);
assign : variable Assign expression -> ^(Assign variable expression);
funcall : variable LP (expression (C expression)*)? RP -> ^(Call variable ^(Parameters expression*));
parameters : (Local (C Local)*)? -> ^(Parameters Local+);
intrinsic : Intrinsic LP expression (C expression)? RP -> ^(Intrinsic expression+);
fundef : Definition variable? LP parameters RP LB (statement SC)* RB -> ^(Definition ^(Name variable) parameters ^(Body statement*));
~~~~

_Globals_ are going to end up in the exported symbols of the compiled binary, so have a different set of restrictions on what characters they can contain. _Locals_ are source-level and private symbols so they are essentially arbitrary (but I'm going to enforce a ruby-style variable naming convention). 

~~~~
@Antlr Tokens@ +=
LP : '('; RP : ')'; C : ','; SC : ';'; LB : '{'; RB : '}'; 
Definition : 'fn'; Return : 'return'; Assign : '=';
Local : ('a'..'z') ('a'..'z'|'_'|'0'..'9')*;
Global : ('A'..'Z'|'_') ('A'..'Z'|'_'|'0'..'9')*;
Number : ('0'..'9')+;
Intrinsic : '+' | '-' | '*';
WS : (' ' | '\t' | '\r' | '\n') {$channel=HIDDEN;};
@Synthetic Tokens@ +=
tokens { Parameters; Call; Body; Root; Name; } // for AST re-writing
~~~~

Overall structure of the Java code

~~~~
@com/blogspot/remisthoughts/compiletoasm/Compiler.java:*@ +=
package com.blogspot.remisthoughts.compiletoasm;
@Imports@
public final class Compiler {
  public static void compile(InputStream srcIn, OutputStream asmOut) throws Exception {
    @Build The AST@ 
    @First Pass@
    @Write The Output@
  }
  @Static Classes@
  @Instruction Classes@
  @Antlr Helpers@
  @Other Helpers@
}
~~~~

## (I) AST To Assembly Instructions ## 

[Useful lecture notes](http://www.classes.cs.uchicago.edu/archive/2011/spring/22620-1/docs/handout-03.pdf)
The first pass: turning an AST into pseudo-assembly, with variables, not registers. Each function should be written into its own _List_ of _Instructions_, and we'll have an extra _List_ for _Instructions_ to go at the end of the _text_ section. We'll flatten nested functions - creating randomly-generated _Labels_ for anonymous functions as needed.

~~~~
@First Pass@ +=
List<List<Instruction>> text = Lists.newArrayList();
text.add(Arrays.asList(Compiler.text, Compiler.align));
for(Tree child : children(ast)) {
  parseDefinition(child, text);
}
~~~~

Objects to represent the subset of assembly we'll generate. Note that these may correspond to more than one x86 assembly instruction (but only a contiguous series of instructions).

~~~~
@Instruction Classes@ +=
abstract static class Instruction { @Instruction Members@ }
static final class Definition extends Instruction {
  final Label name; final boolean isGlobal; 
  @Definition Members@
  Definition(Label name, boolean isGlobal) { this.name = name; this.isGlobal = isGlobal; }
  public String toString() { @Definition x86 Code@ }
}
enum Op { 
  addq("+") { @ADDQ@ }, 
  movq("⇒") { @MOVQ@ }, 
  subq("-") { @SUBQ@ },
  divq("/") { @DIVQ@ },
  mulq("*") { @MULQ@ },
  andq("&") { @ANDQ@ },
  orq("|")  { @ORQ@ };
  Instruction with(Value arg1, ValueCanStoreInto arg2) { 
    return new BinaryOp(this, arg1, arg2); 
  }
  @Op Members@
};
static final class BinaryOp extends Instruction {
  final Value arg1; final ValueCanStoreInto arg2; final Op op;
  BinaryOp(Op op, Value arg1, ValueCanStoreInto arg2) { this.op = op; this.arg1 = arg1; this.arg2 = arg2; }
  public String toString() { @BinaryOp x86 Code@ }
  @BinaryOp Members@
}
~~~~

Maybe a note on (code) alignment?

~~~~
@Other Helpers@ +=
private static final Instruction align = new NoArgInstruction(@Align x86 Code@);
~~~~

The two code sections

~~~~
@Instruction Classes@ +=
private static class NoArgInstruction extends Instruction {
  private final String x86Code;
  NoArgInstruction( String x86Code ) { this.x86Code = x86Code; }
  public String toString() { return x86Code; }
}
private static final Instruction text = new NoArgInstruction(@Text x86 Code@);
private static final Instruction data = new NoArgInstruction(@Data x86 Code@);
~~~~

Generate x86 GAS code - you could probably use a factory to create the _Instruction_ objects if you wanted to support more than one architecture. See [this](http://stackoverflow.com/a/2752639/42543) for why _.type_ isn't supported on mac. See [lib-c free](https://blogs.oracle.com/ksplice/entry/hello_from_a_libc_free). https://developer.apple.com/library/mac/#documentation/DeveloperTools/Reference/Assembler/000-Introduction/introduction.html . [Instruction set (pdf)](http://download.intel.com/design/intarch/manuals/24319101.pdf). 

~~~~
@Static Classes@ +=
interface Value {}
@Value Classes@
static final class Label implements ValueCanStoreInto {
  final String name;
  Label(String name) { this.name = name; }
  Label() { this( "LABEL" + uniqueness.getAndIncrement()); } // for anonymous fns
  public String toString() { return name; }
}
static final class Immediate implements Value {
  final UnsignedLong value;
  Immediate(UnsignedLong value) { this.value = value; }
  Immediate(long value) { this.value = UnsignedLong.asUnsigned(value); }
  public boolean equals(Object o) { return o instanceof Immediate && ((Immediate)o).value.equals(value);}
  public String toString() { @Immediate x86 Code@ }
}
private interface ValueCanStoreInto extends Value {}
static final class Variable implements ValueCanStoreInto, Comparable<Variable> {
  final String name;
  Variable(String name) { this.name = name; }
  Variable() { this("VAR" + uniqueness.getAndIncrement()); };
  public String toString() { return name; }
  @Variable Members@
}
private static final AtomicInteger uniqueness = new AtomicInteger();
~~~~

The registers (and their types). NB: omit frame pointer here - and add _THESTACK_ as a synthetic var.

~~~~
@Value Classes@ +=
enum Register implements ValueCanStoreInto {
  rax, rbx, rcx, rdx, rsp /*stack pointer*/, 
  rbp /*frame pointer*/, rsi, rdi, r8, r9, 
  r10, r11, r12, r13, r14, r15, THESTACK;
  public String toString() { return String.format("%%%s", name()); }
  private static final Register[] ASSIGNABLE = { 
    rax, rbx, rcx, rdx, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15, THESTACK 
  };
  private static final Register[] CALLEE_SAVES = { rbx, rbp, r12, r13, r14, r15 };
  private static final Register[] PARAMETERS = { rdi, rsi, rdx, rcx, r8, r9 };
}
~~~~

Calling functions [CDECL](http://en.wikipedia.org/wiki/X86_calling_conventions#cdecl). Note that we'll assume the function being called is a local variable if it has the same name as a local variable in this function - otherwise it'll be a label which the linker will resolve for us.

~~~~
@Parse A Call Statement@ +=
call(statement, code, text, params, declaredVars);
@Other Helpers@ +=
private static void call(Tree t, List<Instruction> code, List<List<Instruction>> text, Map<Variable, Parameter> params, Set<Variable> declaredVars) {
  List<Value> values = Lists.newArrayList();
  for (Tree argument : children(t, 1)) {
    values.add(getAsValue(argument, code, text, params, declaredVars));
  }
  int numParams = values.size(), maxInRegs = Register.PARAMETERS.length;
  for(int r = 0; r < Math.min(numParams, maxInRegs); ++r)
    code.add(Op.movq.with(values.get(r), Register.PARAMETERS[r]));
  @Before We Push Function Params Onto Stack@
  for(Value value : Lists.reverse(values.subList(Math.min(numParams, maxInRegs), numParams))) 
    code.add(@Push Value Onto The Stack@);
  Variable fnName = new Variable(text(t, 0));
  code.add(new Call(declaredVars.contains(fnName) ? fnName : new Label("_" + fnName.name)));
  if(numParams > maxInRegs) { @Remove Function Call Params From Stack@ }
}
@Instruction Classes@ +=
private static final class Call extends Instruction {
  final Value name;
  Call(Value name) { this.name = name; }
  public String toString() { @Call x86 Code@ }
}
~~~~

According to [the ABI (pdf)](http://www.x86-64.org/documentation/abi.pdf), values are returned in the _rax_ register.

~~~~
@Parse A Return Statement@ +=
Value ret = getAsValue(get(statement,0), code, text, params, declaredVars);
code.add(Op.movq.with(ret, Register.rax));
@Returning From Function@
~~~~

..and after we've returned:

~~~~
@Call A Function And Keep the Return Value@ +=
call(t, code, text, params, declaredVars);
Variable var = new Variable();
code.add(Op.movq.with(Register.rax, var));
return var;
~~~~

Assignment (to locals and globals)

~~~~
@Parse An Assign Statement@ +=
Value expr = getAsValue(get(statement, 1), code, text, params, declaredVars);
ValueCanStoreInto to;
if(type(statement, 0) == Global) {
    to = new Label(text(statement, 0));
} else {
    to = new Variable(text(statement, 0));
    @Assigned To A Local Variable@
}
code.add(Op.movq.with(expr, to));
~~~~

Built-ins: one symbol corresponds to an assembly instruction. We'll assume they only have two 

~~~~
@Op Members@ +=
final String c;
Op(String c) { this.c = c; }
private static Op parse(String c) { 
  for(Op op : values()) if(Objects.equal(op.c, c)) return op;
  throw new IllegalArgumentException();  
}
@Parse An Intrinsic Call@ +=
Value arg1 = getAsValue(get(t, 0), code, text, params, declaredVars);
Value arg2 = getAsValue(get(t, 1), code, text, params, declaredVars);
Variable ret = new Variable();
code.add(Op.movq.with(arg1, ret));
code.add(Op.parse(text(t)).with(arg2, ret));
return ret;
~~~~

~~~~
@Other Helpers@ += 
private static Value getAsValue(Tree t, List<Instruction> code, List<List<Instruction>> text, Map<Variable, Parameter> params, Set<Variable> declaredVars) {
      switch (t.getType()) {
        case Definition:
            return parseDefinition(t, text);
        case Global:
            return new Label(text(t));
        case Local:
            Value v = new Variable(text(t));
            @Check If Variable Is A Parameter@
            return v;
        case Call:
            @Call A Function And Keep the Return Value@ 
        case Intrinsic:
            @Parse An Intrinsic Call@
        case Number :
            return new Immediate(UnsignedLong.valueOf(t.getText()));
        default :
            throw new IllegalArgumentException();
    }
}
~~~~

Functions

What if they're anonymous? Need "_" for C linkage

~~~~
@Get Function Name@ +=
Label myName = hasChildren(def, 0) ? new Label("_" + text(def, 0, 0)) : new Label();
code.add(new Definition(myName, hasChildren(def, 0) && type(def, 0, 0) == Global));
@Is This The Main Function@
~~~~

~~~~
@Other Helpers@ +=
private static Label parseDefinition(Tree def, List<List<Instruction>> text) {
  List<Instruction> code = Lists.newArrayList(); text.add(code);
  @Get Function Name@
  @Entering Function@
  @Parse Function Parameters@
  @Keep Track Of Declared Variables@
  for (Tree statement : children(def, 2)) {
    switch (type(statement)) {
      case Call :   @Parse A Call Statement@ break;
      case Return : @Parse A Return Statement@ break;
      case Assign : @Parse An Assign Statement@ break;
    }
  }
  if(getLast(code) != ret) { @Returning From Function@ } 
  @After Function Defined@
  return myName;
}
~~~~

Keeping track of user-specified (not auto-inserted) variables in the code. This is for function calls - how do we tell if the string function "name" is a symbol or a variable for dynamic dispatch? We'll assume symbol unless clearly a var as we don't have imports or other ways to tell what symbols can be dynamically linked

~~~~
@Keep Track Of Declared Variables@ +=
Set<Variable> declaredVars = Sets.newTreeSet(params.keySet());
@Assigned To A Local Variable@ +=
declaredVars.add((Variable)to);
~~~~

Stack frames. [Enter](http://www.read.seas.harvard.edu/~kohler/class/04f-aos/ref/i386/ENTER.htm)
http://www.agner.org/optimize/calling%5Fconventions.pdf http://blogs.embarcadero.com/eboling/2009/05/20/5607
Note that CALLEE_SAVE_VAR shouldn't be user-enterable or this will confuse things, hence "!"

~~~~
@Instruction Classes@ +=
static final Instruction ret = new NoArgInstruction(@Ret x86 Code@) { @Ret Members@ };
@Entering Function@ +=
for(Register r : Register.CALLEE_SAVES)
  code.add(Op.movq.with(r, new Variable(CALLEE_SAVE_VAR + r)));
@Returning From Function@ +=
for(Register r : Register.CALLEE_SAVES)
  code.add(Op.movq.with(new Variable(CALLEE_SAVE_VAR + r), r));
@Are We Returning From The Main Function@
code.add(Compiler.ret);
@Other Helpers@ +=
private static final String CALLEE_SAVE_VAR = "save!";
~~~~

Dealing with parameters. Make _Variable_ comparable and add _equals_ methods so we can use it as a key to a collection. Note thatsome are passed in registers and others on the stack.

~~~~
@Parse Function Parameters@ +=
Map<Variable, Parameter> params = Maps.newTreeMap();
int number = 0;
for(Tree param : children(def, 1)) {
  Variable v = new Variable(text(param));
  if(number < Register.PARAMETERS.length)
    code.add(Op.movq.with(Register.PARAMETERS[number++], v));
  else
    params.put(v, new Parameter(number++ - Register.PARAMETERS.length));
}
@Static Classes@ +=
private static final class Parameter implements ValueCanStoreInto {
  final int number; // 0..n-1
  Parameter(int number) { this.number = number; }
  public String toString() { @Parameter x86 Code@ }
  @Parameter Members@
}
@Check If Variable Is A Parameter@ +=
v = params.containsKey(v) ? params.get(v) : v;
@Variable Members@ +=
public boolean equals(Object o) { return o instanceof Variable && ((Variable)o).name.equals(name);}
public int compareTo(Variable o) { return name.compareTo(o.name); }
~~~~

Jumping about

~~~~
@Instruction Classes@ +=
private static class DefineLabel extends Instruction {
  final Label label;
  DefineLabel( Label label ) { this.label = label; }
  public String toString() { @Define Label x86 Code@ }
  public boolean equals(Object o) { return o instanceof DefineLabel && ((DefineLabel)o).label.equals(label); }
}
~~~~

OSX demands16-byte alignment when we call a function. Unfortunately, when we start at the _main_ entrypoint, the stack is pretty arbitary:

<pre>
Starting program: /private/var/folders/HT/HTSjwIiwHaSNfbnpJnpWX++++TI/-Tmp-/2859 
Breakpoint 1, 0x0000000100010000 in main ()
(gdb) info register rsp
rsp            0x7fff5fbff858   0x7fff5fbff858
(gdb)
</pre>

However, when we return from main we should restore the original _rsp_ (regardless of its alignment) otherwise we'll segfault.

~~~~
@Is This The Main Function@ +=
if(myName.name.equals("_main")) {
  code.add(new Push(rsp));
  code.add(Op.andq.with(new Immediate(0xFFFFFFFFFFFFFFF0L), rsp));
}
@Are We Returning From The Main Function@ +=
if(myName.name.equals("_main")) code.add(new Pop(rsp));
@Instruction Classes@ +=
private static final class Push extends Instruction {
  final Value value;
  Push(Value value) { this.value = value; }
  public String toString() { @Push x86 Code@ }
  @Push Members@
}
private static final class Pop extends Instruction {
  final ValueCanStoreInto value;
  Pop(ValueCanStoreInto value) { this.value = value; }
  public String toString() { @Pop x86 Code@ }
}
~~~~

...and when we're calling a function, we should preserve stack alignment, so if we're pushing an odd number of args onto the stack we'll have to pad the stack with 8 bytes first to leave it at a 16 byte boundary. 

~~~~
@Before We Push Function Params Onto Stack@ += 
if(numParams > maxInRegs && (numParams - maxInRegs) % 2 != 0)
  code.add(Op.subq.with(new Immediate(8), rsp));
@Remove Function Call Params From Stack@ +=
int correction = 0;
if((numParams - maxInRegs) % 2 != 0) correction = 1;
code.add(Op.addq.with(new Immediate((numParams - maxInRegs + correction) * 8), rsp));
~~~~

## (II) Register Allocation via Linear Programming ##

We'll do this function-by-function
http://www.xypron.de/projects/linopt/apidocs/index.html (the paper)[http://grothoff.org/christian/lcpc2006.pdf]

~~~~
@After Function Defined@ +=
code = registerAllocation(code);
@Other Helpers@ +=
static List<Instruction> registerAllocation(final List<Instruction> code) {
  @Get Variables Used@
  @Build Control Flow Graph@
  @Calculate Liveness@
  @LP Initialisation@
  @Set Up Columns@
  @Set Up Objective@
  @Set Up Constraints@
  @Solve@
  @Copy Solution Into Code@
  @Insert Stack Moves@
  @After Register Allocation@
  return code;
}
~~~~

We'll only assign to the x86's general purpose registers:

Each instruction has a node before it and a node after it, but instructions may share nodes with each other due to branching. 

The control flow graph has to take into account conditional and unconditional jumps, so _Instructions_ need to provide information about where control flow goes next. 

~~~~
@Instruction Members@ +=
boolean isNextInstructionReachable() { return true; }
Label couldJumpTo() { return null; }
@Ret Members@ +=
boolean isNextInstructionReachable() { return false; }
~~~~

We need two separate bits of information from this graph; how the nodes relate to each other and how the instructions fit in between the nodes. 

~~~~
@Other Helpers@ +=
static final class ControlFlowGraph {
  @Control Flow Graph Members@
  ControlFlowGraph(Variable[] variables, List<Instruction> code) {
    @Store Number Of Variables@
    @Fit Instructions Between Nodes@
    @Map Nodes To Instructions@
    @Join Nodes Together@
    @Variables Used At Next Instruction@
    @Identify Function Calls@
  }
}
@Build Control Flow Graph@ +=
ControlFlowGraph controlFlow = new ControlFlowGraph(variables, code);
~~~~

Firstly: how nodes relate to the graph. This will create new nodes (i.e. increment _highestNode_) as needed. It's important we check both branches, as even though we'll only set our next node once (even if we branch) we want to make sure every node has a previous node set.

~~~~
@Control Flow Graph Members@ +=
private static final int FIRST_NODE = 0, LAST_NODE = 1, UNASSIGNED = -1;
final int numNodes;
final int[] prevNode, nextNode;
@Fit Instructions Between Nodes@ +=
int highestNode = LAST_NODE + 1;
prevNode = new int[code.size()]; Arrays.fill(prevNode, 1, code.size(), UNASSIGNED);
nextNode = new int[code.size()]; Arrays.fill(nextNode, UNASSIGNED);
for(int i = 0; i < code.size(); ++i ) {
  Instruction instr = code.get(i);
  if(instr.isNextInstructionReachable()) {
    if(prevNode[i+1] == UNASSIGNED) prevNode[i+1] = highestNode++; 
    nextNode[i] = prevNode[i+1];
  }
  if(instr.couldJumpTo() != null) {
    int nextInstr = code.indexOf(new DefineLabel(instr.couldJumpTo()));
    if(prevNode[nextInstr] != UNASSIGNED) nextNode[i] = prevNode[nextInstr]; // e.g. a loop
    else nextNode[i] = prevNode[nextInstr] = highestNode++; 
  }
  if(nextNode[i] == UNASSIGNED) nextNode[i] = LAST_NODE; // control terminates, e.g. a ret
}
this.numNodes = highestNode;
~~~~

Now we know where nodes fit into the graph we can build up a map of which instructions directly follow each node. We'll use this after we've decided what variables we need to spill or load from the stack as we'll need to know where to insert the instructions to do this.

~~~~
@Control Flow Graph Members@ +=
final Multimap<Integer, Integer> 
  instructionsFollowingNode = HashMultimap.create(), 
  instructionsBeforeNode = HashMultimap.create(); 
@Map Nodes To Instructions@ +=
for(int i = 0; i < code.size(); ++i ) {
  instructionsFollowingNode.put(prevNode[i], i);
  instructionsBeforeNode.put(nextNode[i], i);
}
~~~~

...and a map of nodes to nodes

~~~~
@Control Flow Graph Members@ +=
ImmutableMultimap<Integer, Integer> nextNodes;
@Join Nodes Together@ +=
ImmutableMultimap.Builder<Integer, Integer> nextNodes = ImmutableMultimap.builder();
for(int i = 0; i < code.size(); ++i )
  nextNodes.put(prevNode[i], nextNode[i]);
this.nextNodes = nextNodes.build();
~~~~

Which variables are used in any of the instructions following this node? We can't call a variable dead if at least one code path reads it, but we can call it dead if every code path writes to it (and doesn't read from it in the same instruction).

~~~~
@Control Flow Graph Members@ +=
BitSet varsReadNext = new BitSet(), varsWrittenNext = new BitSet();
@Variables Used At Next Instruction@ +=
for(int i = 0; i < code.size(); ++i )
  for(Variable variable : filter(code.get(i).readsFrom(), Variable.class))
    varsReadNext.set(prevNode[i] * variables.length + indexOf(variables, variable));
for(Entry<Integer, Collection<Integer>> next : instructionsFollowingNode.asMap().entrySet()) { // not empty
  Set<Variable> varsSet = Sets.newTreeSet(Arrays.asList(variables)); 
  for(int i : next.getValue())
    varsSet.retainAll(Sets.newTreeSet(filter(code.get(i).writesTo(), Variable.class)));
  for(Variable variable : varsSet)
    varsWrittenNext.set(next.getKey() * variables.length + indexOf(variables, variable));
}
~~~~

If one of the instructions following a node is a function call, we should only put live variables into registers that'll be preserved across function calls.

~~~~
@Control Flow Graph Members@ +=
BitSet nodesBeforeFnCalls = new BitSet();
@Identify Function Calls@ += 
for(int i = 0; i < code.size(); ++i )
  if(code.get(i) instanceof Call)
    nodesBeforeFnCalls.set(prevNode[i]);
~~~~

Data dependencies - will let us calculate liveness

~~~~
@Instruction Members@ +=
Set<Value> readsFrom() { return Collections.emptySet(); }
Set<ValueCanStoreInto> writesTo() { return Collections.emptySet(); }
<E> Set<E> uses(Class<E> c) { return ImmutableSet.copyOf(filter(concat(readsFrom(), writesTo()), c)); }
@BinaryOp Members@ +=
Set<Value> readsFrom() { return op.readsFrom(arg1, arg2); }
Set<ValueCanStoreInto> writesTo() { return op.writesTo(arg1, arg2); }
@Op Members@ +=
Set<Value> readsFrom(Value arg1, ValueCanStoreInto arg2) { return ImmutableSet.of(arg1, arg2); }
Set<ValueCanStoreInto> writesTo(Value arg1, ValueCanStoreInto arg2) { return  ImmutableSet.of(arg2); }
@MOVQ@ +=
Set<Value> readsFrom(Value arg1, ValueCanStoreInto arg2) { return ImmutableSet.of(arg1); }
~~~~

We need to know all the variables we'll ever need to assign

~~~~
@Get Variables Used@ +=
Set<Variable> allVariables = Sets.newTreeSet();
for(Instruction i : code) {
  addAll(allVariables, filter(i.readsFrom(), Variable.class));
  addAll(allVariables, filter(i.writesTo(), Variable.class));
}
if(allVariables.isEmpty()) { @No Stack Variables@ return code; }
Variable[] variables = allVariables.toArray(new Variable[allVariables.size()]);
~~~~

Liveness...as a BitSet

~~~~
@Other Helpers@ +=
static final class Liveness {
  final Variable[] variables; final BitSet isLive;
  Liveness(ControlFlowGraph controlFlow) { 
    this.variables = controlFlow.variables;
    @Calculate Liveness From Graph@
  }
  boolean isLiveAt(int variable, int node) {
    return isLive.get(node * variables.length + variable);
  }
  public String toString() {
    StringBuilder ret = new StringBuilder();
    for (int v = 0; v < variables.length; ++v) {
      ret.append(Strings.padStart(variables[v].toString(), 20, ' ')).append(':');
      for (int n = 0; n < (isLive.length()+1) / variables.length; ++n) 
        ret.append(isLive.get(n * variables.length + v) ? "-" : " ");
      ret.append('\n');
    }
    return ret.toString();
  }
  @Liveness Members@
}
~~~~

A variable is not live at the node before the instruction where it's created and is not live at the node after the instruction where it's last read.

~~~~
@Calculate Liveness From Graph@ +=
isLive = (BitSet) controlFlow.varsReadNext.clone(); 
Queue<Integer> unprocessed = Queues.newArrayDeque(Arrays.asList(ControlFlowGraph.LAST_NODE));
BitSet nodesSeen = new BitSet();
for(Integer node = unprocessed.poll(); node != null; node = unprocessed.poll()) {
  boolean changed = false;
  for(int n : controlFlow.nextNodes.get(node))
      changed |= copyFrom(isLive, n, node, variables.length);
  for(int index = node * variables.length; index < (node+1) * variables.length; ++index)
    if(controlFlow.varsWrittenNext.get(index) && !controlFlow.varsReadNext.get(index))
      isLive.clear(index);
  if(!nodesSeen.get(node) || changed)
    for(int n : controlFlow.nextNodes.inverse().get(node)) unprocessed.offer(n);
  nodesSeen.set(node);
}
@Calculate Liveness@ +=
Liveness liveness = new Liveness(controlFlow); 
~~~~

We can use liveness information to decide what variables we need to assign to registers at each node. At function calls we should ensure that any live variables are stored in registers that'll be preserved. The assembly instructions inserted before the function call itself will ensure that the parameters will stay in the correct registers. Also, if a variable is going to be used in an instruction following a node that variable can't be assigned to the stack!

~~~~
@Control Flow Graph Members@ +=
final Variable[] variables;
boolean needsAssigning(Liveness liveness, int variable, int node, Register r) {
  boolean isLive = liveness.isLiveAt(variable, node) || isVarUsedNext(variable, node);
  boolean thisRegPreserved = !nodesBeforeFnCalls.get(node) || calleeSavesRegisterIndices.get(r.ordinal());
  boolean ifUsedNotOnStack = !(isVarUsedNext(node, variable) && r == Register.THESTACK);
  return isLive && thisRegPreserved && ifUsedNotOnStack;
}
@Store Number Of Variables@ +=
this.variables = variables; 
@Other Helpers@ += 
private static final BitSet calleeSavesRegisterIndices = new BitSet();
static {
  for(Register r : Register.CALLEE_SAVES)
    calleeSavesRegisterIndices.set(r.ordinal());
  calleeSavesRegisterIndices.set(Register.THESTACK.ordinal()); // the stack's saved too!
}
~~~~

Now we can start setting up the problem. Note that some constraints refer to just the assignable registers, and others refer to our dummy register indicating the stack:

~~~~
@LP Initialisation@ += 
Problem allocation = new Problem();
~~~~ 

The first (and most important) set of unknowns are the _VAR_IN_REG_AT_INSTR_ columns: which register each variable is in at each node in the control flow graph, assuming it's live. 

~~~~
@Set Up Columns@ +=
for (int v = 0; v < variables.length; ++v)
  for(int n = 0; n < controlFlow.numNodes; ++n)
    for (Register r : Register.ASSIGNABLE)
      if(controlFlow.needsAssigning(liveness, v, n, r))
        allocation.column(VAR_IN_REG_AT_INSTR, v, r.ordinal(), n).type(ColumnType.BINARY);
@Other Helpers@ +=
private static final String VAR_IN_REG_AT_INSTR = "x";
~~~~

The next set of unknowns are used to work out the cost of a given assignment. We'll use constraints to ensure that the _NEW_VAR_IN_REG_FLAG_ columns have a one for node _n_, register _r_ and variable _v_ if register _r_ had a different variable in before node _n_ and now has variable _v_ in, and in all other circumstances _NEW_VAR_IN_REG_FLAG_ is zero. This makes the variable an indicator of whether we've loaded a variable into a register from the stack, so it'll be an important part of the objective function (as we want to minimise stack usage).

~~~~
@Set Up Columns@ +=
for (int v = 0; v < variables.length; ++v)
  for(int n = 0; n < controlFlow.numNodes; ++n)
    for (Register r : Register.ASSIGNABLE)
      if(controlFlow.needsAssigning(liveness, v, n, r))
        allocation.column(NEW_VAR_IN_REG_FLAG, v, r.ordinal(), n).type(ColumnType.BINARY);
@Other Helpers@ +=
private static final String NEW_VAR_IN_REG_FLAG = "c";
~~~~

The cost of register access, _mu_ in (the paper)[http://grothoff.org/christian/lcpc2006.pdf], can be a function of the register and the instruction. This gives us the option of discouraging moves in frequently-used instructions (such as those in the body of a loop). Note that we don't check for liveness here as we don't even think about preserving a non-live variable.

~~~~
@Other Helpers@ +=
private static final double COST_OF_REG_ACCESS = 1.0;
private static double spillCost(int variable, int node, ControlFlowGraph controlFlow) {
  double ret = 0.0;
  if(controlFlow.isVarUsedNext(variable, node)) ret += COST_OF_SPILL;
  if(controlFlow.isVarUsedPrev(variable, node)) ret += COST_OF_SPILL;
  return ret;
}
private static final double COST_OF_SPILL = 100.0;
@Control Flow Graph Members@ +=
boolean isVarUsedNext(int variable, int node) {
  return varsReadNext.get(node * variables.length + variable) 
      || varsWrittenNext.get(node * variables.length + variable);
}
boolean isVarUsedPrev(int variable, int node) {
  for(int n : nextNodes.get(node))
    if(isVarUsedNext(variable, n))
      return true;
  return false;
}
~~~~

The objective: minimise spillage at each node. 

~~~~
@Set Up Objective@ +=
allocation.objective("spillage", Direction.MINIMIZE);
@Minimise Number Of Spills@
for (int v = 0; v < variables.length; ++v)
  for (int n = 0; n < controlFlow.numNodes; ++n)
    for (Register r : Register.ASSIGNABLE)
      if(controlFlow.needsAssigning(liveness, v, n, r))
        allocation.objective().add(COST_OF_REG_ACCESS, NEW_VAR_IN_REG_FLAG, v, r.ordinal(), n);
@Add Objective Hints@
~~~~

~~~~
@Minimise Number Of Spills@ +=
for (int v = 0; v < variables.length; ++v)
  for (int n = 0; n < controlFlow.numNodes; ++n)
    if(controlFlow.needsAssigning(liveness, v, n, Register.THESTACK))
      allocation.objective().add(spillCost(v, n, controlFlow), VAR_IN_REG_AT_INSTR, v, Register.THESTACK.ordinal(), n);
~~~~

Hinting. We'd like to make as many _movq_s no-ops as possible (especially if we've defined a register). We deliberately insert a move just before a return to set the _eax_ register to the return value; it'd be nice if it was a no-op. Also, after a _call_ where we move the return value in _eax_ to the return _Variable_. However, currently:
<pre>
fn() { a=1; b=5; return a+b; }
</pre>
Compiles to:
<pre>
movl $1, %eax
movl $5, %ebx
movl %eax, %ecx
addl %ebx, %ecx
movl %ecx, %eax
</pre>

~~~~
@Add Objective Hints@ +=
for(int i = 0; i < code.size(); ++i) {
  if(!isAMove(code.get(i))) continue;
  for(Variable v : code.get(i).uses(Variable.class))
    for(Register r : code.get(i).uses(Register.class))
      for(int n : controlFlow.nodesAround(i)) 
        if(controlFlow.needsAssigning(liveness, indexOf(variables, v), n, r))
          allocation.objective().add(MOVE_NOP_HINT, VAR_IN_REG_AT_INSTR, indexOf(variables, v), r.ordinal(), n);
}
@Other Helpers@ +=
private static boolean isAMove(Instruction i) {
  return i instanceof BinaryOp && ((BinaryOp)i).op == Op.movq;
}
private static final double MOVE_NOP_HINT = -0.5;
@Control Flow Graph Members@ +=
List<Integer> nodesAround(int instruction) {
  return Arrays.asList(prevNode[instruction], nextNode[instruction]);
}
~~~~

We now get 
<pre>
movl $1, %ebx
movl $5, %ecx
movl %ebx, %eax
addl %ecx, %eax
movl %eax, %eax
</pre>

Constraint: registers can only ever hold one variable at once (apart from the stack). 

~~~~
@Control Flow Graph Members@ +=
Set<Integer> getAliasingVars(List<Instruction> code, Liveness liveness, int node) {
  Collection<Integer> instrsAfter = instructionsFollowingNode.get(node);
  if(instrsAfter.size() != 1) return Collections.emptySet();
  int i = getOnlyElement(instrsAfter); Instruction instr = code.get(i); 
  if(!isAMove(instr)) return Collections.emptySet();
  if(instr.uses(Variable.class).size() != 2) return Collections.emptySet();
  int reads = indexOf(variables, getOnlyElement(instr.readsFrom()));
  if(liveness.isLiveAt(reads, nextNode[i])) return Collections.emptySet();
  int writes = indexOf(variables, getOnlyElement(instr.writesTo()));
  if(liveness.isLiveAt(writes, node)) return Collections.emptySet();
  return ImmutableSet.of(reads, writes);
}
~~~~

~~~~
@Set Up Constraints@ +=
for (int n = 0; n < controlFlow.numNodes; ++n) {
  for (Register r : Register.ASSIGNABLE)
    if(r != Register.THESTACK)
      if(controlFlow.getAliasingVars(code, liveness, n).isEmpty()) {
        for (int v = 0; v < variables.length; ++v)
          if(controlFlow.needsAssigning(liveness, v, n, r))
            allocation.row(ONE_VAR_PER_REG, r.ordinal(), n)
              .bounds(0.0, 1.0)
              .add(1.0, VAR_IN_REG_AT_INSTR, v, r.ordinal(), n);
      } else {
        for(int avoid : controlFlow.getAliasingVars(code, liveness, n))
          for (int v = 0; v < variables.length; ++v)
            if(v != avoid && controlFlow.needsAssigning(liveness, v, n, r))
              allocation.row(ONE_VAR_PER_REG, r.ordinal(), n, avoid)
                .bounds(0.0, 1.0)
                .add(1.0, VAR_IN_REG_AT_INSTR, v, r.ordinal(), n);
      }
  }
@Other Helpers@ +=
private static final String ONE_VAR_PER_REG = "one_var_per_reg";
~~~~

Constraint: variables must not be lost; they must be in either the stack or exactly one register. While technically we could put the variable on stack as well as in more than one register, this makes the LP solver much slower as there are so many more possible combinations of assignments to narrow down. Also, if a variable's not live then force it on the stack.

~~~~
@Set Up Constraints@ +=
for (int v = 0; v < variables.length; ++v)
  for (int n = 0; n < controlFlow.numNodes; ++n)
    for (Register r : Register.ASSIGNABLE)
      if(controlFlow.needsAssigning(liveness, v, n, r))
        allocation.row(VARS_NOT_LOST, v, n)
          .bounds(1.0, 1.0)
          .add(1.0, VAR_IN_REG_AT_INSTR, v, r.ordinal(), n);
@Other Helpers@ +=
private static final String VARS_NOT_LOST = "vars_not_lost";
~~~~

The last pair of constraints ensure that the _NEW_VAR_IN_REG_FLAG_ works as we want. What's the -1 for?

~~~~
@Set Up Constraints@ +=
for (int v = 0; v < variables.length; ++v)
  for (Register r : Register.ASSIGNABLE)
    for(Entry<Integer, Integer> n : controlFlow.nextNodes.entries()) {
      if(!controlFlow.needsAssigning(liveness, v, n.getKey(), r) 
      || !controlFlow.needsAssigning(liveness, v, n.getValue(), r)) 
        continue;
      allocation.
        row(FLAG_SET_CORRECTLY, v, r.ordinal(), n.getKey(), n.getValue()).
        bounds(0.0, null).
        add( 1.0, VAR_IN_REG_AT_INSTR, v, r.ordinal(), n.getValue()).
        add(-1.0, VAR_IN_REG_AT_INSTR, v, r.ordinal(), n.getKey()).
        add( 1.0, NEW_VAR_IN_REG_FLAG, v, r.ordinal(), n.getValue());
      allocation.
        row(FLAG_SET_CORRECTLY, v, r.ordinal(), n.getValue()-1, n.getKey()).
        bounds(0.0, null).
        add(-1.0, VAR_IN_REG_AT_INSTR, v, r.ordinal(), n.getValue()).
        add( 1.0, VAR_IN_REG_AT_INSTR, v, r.ordinal(), n.getKey()).
        add( 1.0, NEW_VAR_IN_REG_FLAG, v, r.ordinal(), n.getValue());
    }
@Other Helpers@ +=
private static final String FLAG_SET_CORRECTLY = "flag_set_correctly";
~~~~

~~~~
@Solve@ +=
if (!solver.solve(allocation))
  throw new IllegalStateException();
@Other Helpers@ +=
private static final Solver solver = new SolverGlpk();
~~~~

We know which register each variable is in at each node now (assuming it's live):

~~~~
@Other Helpers@ +=
private static Register whereAmI(Problem allocation, int v, int n) {
  for(Register r : Register.ASSIGNABLE)
    if(allocation.column(VAR_IN_REG_AT_INSTR, v, r.ordinal(), n).getValue() > 0.5)
        return r;
  throw new IllegalStateException(); // shouldn't get here
}
~~~~

Now we know where to put variables, we can update the instructions. We'll make the instructions immutable, and so we need copy-on-resolve semantics.

~~~~
@Instruction Members@ +=
Instruction resolve(Variable from, Register to) { return this; }
@BinaryOp Members@ +=
Instruction resolve(Variable from, Register to) { 
  return op.with(from.equals(arg1) ? to : arg1, from.equals(arg2) ? to : arg2); 
}
~~~~

We'll iterate through the linear programming solution and resolve variables as needed. For each instruction, we'll use the node before to work out which variables are where. Note that the variable must be live at the previous node, by definition.

~~~~
@Copy Solution Into Code@ +=
for(int i = 0; i < code.size(); ++i)
  for(Variable variable : code.get(i).uses(Variable.class)) {
    int v = indexOf(variables, variable);
    for (Register r : Register.ASSIGNABLE)
      if(allocation.column(VAR_IN_REG_AT_INSTR, v, r.ordinal(), controlFlow.prevNode[i]).getValue() > 0.5) { // huge tolerance for 0-1 integer
        code.set(i, code.get(i).resolve(variable, r)); break;
      }
  } 
~~~~

The stack. We'll keep it sorted so it has a predicable order (?)

~~~~
@LP Initialisation@ +=
List<Integer> stackVars = Lists.newArrayList();
@Value Classes@ +=
static final class StackVar implements ValueCanStoreInto {
  final int var;
  StackVar(int var) { this.var = var; }
  public String toString() { @StackVar x86 Code@ }
}
@Other Helpers@ +=
private static ValueCanStoreInto onStack(List<Integer> stackVars, int v) {
  if(!stackVars.contains(v)) stackVars.add(v); 
  return new StackVar(stackVars.indexOf(v));
}
~~~~

~~~~
@Copy Solution Into Code@ +=
Multimap<Integer, Instruction> 
  stackMovesBefore = HashMultimap.create(), 
  stackMovesAfter = HashMultimap.create();
for (int v = 0; v < variables.length; ++v)
  for (int n = 0; n < controlFlow.numNodes; ++n)
    if(controlFlow.needsAssigning(liveness, v, n, Register.THESTACK))
      if(allocation.column(NEW_VAR_IN_REG_FLAG, v, Register.THESTACK.ordinal(), n).getValue() > 0.5) // v has moved 
        if(allocation.column(VAR_IN_REG_AT_INSTR, v, Register.THESTACK.ordinal(), n).getValue() > 0.5) // v now on stack
          for(int i : controlFlow.instructionsBeforeNode.get(n))
            stackMovesAfter.put(i, Op.movq.with(whereAmI(allocation, v, controlFlow.prevNode[i]), onStack(stackVars, v)));
        else // v now in register
          for(int i : controlFlow.instructionsFollowingNode.get(n))
            stackMovesBefore.put(i, Op.movq.with(onStack(stackVars, v), whereAmI(allocation, v, n)));
~~~~

Inserting is a bit tricky as the indices will change

~~~~
@Insert Stack Moves@ +=
int drift = 0;
for(int i : Sets.newTreeSet(Sets.union(stackMovesBefore.keySet(), stackMovesAfter.keySet()))) {
  code.addAll(i + drift, stackMovesBefore.get(i)); 
  drift += stackMovesBefore.get(i).size();
  code.addAll(i + drift + 1, stackMovesAfter.get(i)); 
  drift += stackMovesAfter.get(i).size();
}
~~~~

Now we know how many local vars we need, we can reserve the appropriate amount of space

~~~~
@After Register Allocation@ +=
if(!stackVars.isEmpty()) {
  Value incr = new Immediate(8 * stackVars.size());
  code.add(1, Op.subq.with(incr, Register.rsp));
  for(int i = 0; i < code.size(); ++i)
    if(code.get(i) == ret) code.add(i++, Op.addq.with(incr, Register.rsp));
}
~~~~

As we're not using _rbp_ to store the stack frame pointer we'll have to adjust the code we need to access a parameter so we subtract the local variables we've allocated, as well as the offset before the stack frame. The code we generate to call functions is important - as we push values onto the stack this offset will increase. We avoid this by calculating all the _Values_ that are going to be passed as parameters first, before pushing them all onto the stack in one go.

Nudges: before a function call, we push things onto the stack. If we are pushing _Parameters_ then we have to adjust the offset to the stack pointer to account for the pushes we've just done.

~~~~
@Parameter Members@ +=
int offset = 0;
Parameter nudge(int by) {
  Parameter ret = new Parameter(number); ret.offset = offset + by; return ret;
}
@Instruction Members@ +=
Instruction nudge(int by) { return this; }
@BinaryOp Members@ +=
Instruction nudge(int by) { 
  return op.with(
    arg1 instanceof Parameter ? ((Parameter)arg1).nudge(by) : arg1, 
    arg2 instanceof Parameter ? ((Parameter)arg2).nudge(by) : arg2); 
}
@Push Members@ +=
Instruction nudge(int by) { 
  return value instanceof Parameter ? new Push(((Parameter)value).nudge(by)) : this; 
}
@Before We Push Function Params Onto Stack@ +=
int nudgeBy = (numParams - maxInRegs) % 2 == 0 ? 0 : 1;
@Push Value Onto The Stack@ +=
new Push(value).nudge(nudgeBy++)
@Copy Solution Into Code@ +=
if(!stackVars.isEmpty())
  for(int i = 0; i < code.size(); ++i)
    code.set(i, code.get(i).nudge(stackVars.size()));
~~~~

## (III) Writing the Output ##

The x86 syntax we'll use: [addl](http://stackoverflow.com/questions/1619131/addl-instruction-x86) [all instructions](http://en.wikipedia.org/wiki/X86_instruction_listings)

~~~~
@Definition x86 Code@ += return isGlobal ? String.format( ".globl %s\n%s:", name, name) : String.format("%s:", name); 
@BinaryOp x86 Code@ += return String.format( "%s %s, %s", op, arg1, arg2);
@Push x86 Code@ += return String.format( "pushq %s", value);
@Pop x86 Code@ += return String.format( "popq %s", value);
@Call x86 Code@ +=
if(name instanceof Label)
  return String.format( "call %s", name); // call a label
else
  return String.format( "call *%s", name); // indirect function call - code address in a register
@Section x86 Code@ += return String.format( ".%s", name()); 
@StackVar x86 Code@ += return String.format("%s(%%rsp)", var * 8);
@Align x86 Code@ += ".align 8"
@Text x86 Code@ += ".text"
@Data x86 Code@ += ".data"
@Ret x86 Code@ += "ret"
@Define Label x86 Code@ += return String.format("%s:", label.name);
@Immediate x86 Code@ += return String.format("$0x%s", value.toString(16));
@Parameter x86 Code@ += return String.format("%s(%%rsp)", (number + offset + 1) * 8);
~~~~

We'll always write to UTF, even if our assembly only ever has ASCII characters in it. We won't bother indenting the code.

~~~~
@Write The Output@ +=
Writer writer = new OutputStreamWriter(asmOut, Charsets.UTF_8); 
for(Instruction i : filter(concat(text), noNoOps)) writer.append(i + "\n");
writer.close();
~~~~

Getting rid of no-ops (most importantly moves)

~~~~~
@Instruction Members@ += 
boolean isNoOp() { return false; } 
@Other Helpers@ += 
static final Predicate<Instruction> noNoOps = new Predicate<Instruction>() {
  public boolean apply(Instruction i) { return !i.isNoOp(); }
};
@BinaryOp Members@ +=
boolean isNoOp() { return op.isNoOp(arg1, arg2); } 
@Op Members@ +=
public abstract boolean isNoOp(Value arg1, ValueCanStoreInto arg2);
@MOVQ@ +=  
public boolean isNoOp(Value arg1, ValueCanStoreInto arg2) { return arg1.equals(arg2); }
@Zero Is Identity@ +=
public boolean isNoOp(Value arg1, ValueCanStoreInto arg2) { return arg1.equals(new Immediate(0)); }
@ADDQ@ += @Zero Is Identity@
@SUBQ@ += @Zero Is Identity@
@ORQ@ += @Zero Is Identity@
@One Is Identity@ +=
public boolean isNoOp(Value arg1, ValueCanStoreInto arg2) { return arg1.equals(new Immediate(1)); }
@DIVQ@ += @One Is Identity@
@MULQ@ += @One Is Identity@
@All Bits Set Is Identity@ +=
public boolean isNoOp(Value arg1, ValueCanStoreInto arg2) { return arg1.equals(new Immediate(0xFFFFFFFFFFFFFFFFL)); }
@ANDQ@ += @All Bits Set Is Identity@
~~~~

## Appendices ##

~~~~
@Imports@ += 
import static com.blogspot.remisthoughts.compiletoasm.UnsignedLexer.*;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.Register.*;
import static com.google.common.collect.Iterables.*;
import static com.google.common.collect.Multimaps.*;
import static com.google.common.base.Predicates.*;
import java.io.*;
import java.util.*;
import java.util.Map.*;
import java.util.concurrent.atomic.*;
import org.antlr.runtime.ANTLRInputStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.tree.*;
import com.google.common.base.*;
import com.google.common.primitives.*;
import com.google.common.collect.*;
import de.xypron.linopt.*;
import de.xypron.linopt.Problem.*;
~~~~

Antlr code to build AST

~~~~
@Antlr Options@ +=
options {
  output=AST;
  ASTLabelType=CommonTree;
}
@Synthetic Tokens@
@lexer::header { package com.blogspot.remisthoughts.compiletoasm; }
@header { package com.blogspot.remisthoughts.compiletoasm; }
@Build The AST@ +=
CommonTree ast = new UnsignedParser(
  new CommonTokenStream(
    new UnsignedLexer(
      new ANTLRInputStream(srcIn)))).eval().tree;
~~~~

stackoverflow.com/questions/2445008/how-to-get-antlr-3-2-to-exit-upon-first-error

~~~~
@Make Antlr Throw On Failure@ +=
@rulecatch {
  catch (RecognitionException e) { throw e; }
}
@parser::members {
  protected Object recoverFromMismatchedToken(IntStream input, int ttype, BitSet follow) throws RecognitionException {
    throw new MismatchedTokenException(ttype, input);
  }
  public Object recoverFromMismatchedSet(IntStream input, RecognitionException e, BitSet follow) throws RecognitionException {
    throw e;
  }
}
@lexer::members {
  public void reportError(RecognitionException e) {
      throw new RuntimeException(e);
  }
}    
~~~~

Antlr grammar layout:

~~~~
@com/blogspot/remisthoughts/compiletoasm/Unsigned.g:*@ +=
grammar Unsigned;
@Antlr Options@
@Make Antlr Throw On Failure@
@Antlr Tokens@
@Antlr Parse Rules@
~~~~

~~~~
@Antlr Helpers@ +=
private static Tree get(Tree from, int... indices){
  for(int index : indices) from = from.getChild(index);
  return from;
}
private static String text(Tree from, int... indices){ return get(from, indices).getText(); }
private static int type(Tree from, int... indices){ return get(from, indices).getType(); }
private static int numChildren(Tree from, int... indices){ return get(from, indices).getChildCount(); }
private static boolean hasChildren(Tree from, int... indices){ return numChildren(from, indices) > 0; }
private static Iterable<Tree> children(Tree from, int... indices){ 
  Iterable<Tree> ret = (Iterable<Tree>)((BaseTree)get(from, indices)).getChildren();
  return ret == null ? Collections.<Tree>emptyList() : ret; 
}
~~~~

Some array helpers (we do a lot of linear searching, but not enough to warrant sorting for a binary search or building a hash map).

~~~~
@Other Helpers@ +=
private static <T> int indexOf(T[] ts, T t) {
  return Arrays.asList(ts).indexOf(t);
}
/** @return true if any changes */
private static boolean copyFrom(BitSet set, int indexFrom, int indexTo, int num) {
  boolean ret = false;
  for(
    int index = set.nextSetBit(indexFrom * num);
    index >= 0 && index < (indexFrom + 1) * num;
    index = set.nextSetBit(index + 1)) 
  {
    int v = index - indexFrom * num;
    ret |= !set.get(indexTo * num + v);
    set.set(indexTo * num + v);
  }
  return ret;
}
~~~~

tools (universal gcc needed or you'll get linker errors like :
ld: warning: ignoring file /opt/local/lib/gcc47/libgcc_ext.10.5.dylib, missing required architecture i386 in file
ld: warning: ignoring file /opt/local/lib/gcc47/gcc/x86_64-apple-darwin10/4.7.1/libgcc.a, file was built for archive which is not the architecture being linked (i386)
):
port install gcc47 +universal
port select --set gcc mp-gcc47
port install binutils

examining
nm -m -U /System/Library/Frameworks/QTKit.framework/QTKit
gobjdump -f /opt/local/lib/gcc47/libgcc_ext.10.5.dylib 