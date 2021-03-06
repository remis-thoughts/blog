# Writing a Compiler Back-End: ASTs to x86-64 Assembly Using Integer Programming #

## Introduction ##

This is a [Literate](http://en.wikipedia.org/wiki/Literate_programming) Java implementation of a compiler that uses the integer programming from [this paper](http://grothoff.org/christian/lcpc2006.pdf) to generate [x86-64](http://download.intel.com/design/intarch/manuals/24319101.pdf) Assembly code ([AT&T](http://en.wikibooks.org/wiki/X86_Assembly/GAS_Syntax) format) from a toy imperative language. As this article only focuses on code generation, I'll use [antlr](http://www.antlr.org) to generate the lexer & parser from the grammar and will make no attempt to [optimise](https://news.ycombinator.com/item?id=6273085) the code being compiled or produce helpful error messages! 

### The language ###

A programme consists of a series of statements or function definitions. The top-level statements are swept into the _main function (whether or not symbols need a [leading underscore](http://stackoverflow.com/a/1035937/42543) is OS-specific). The grammer uses antlr's [re-write](http://meri-stuff.blogspot.co.uk/2011/09/antlr-tutorial-expression-language.html#RewriteRules) [rules](http://www.antlr.org/wiki/display/~admin/2008/11/30/Example+tree+rewriting+with+patterns) to build the programme's [Abstract Syntax Tree](http://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST).

~~~~
@Antlr Tokens@ +=
SC : ';'; 
@Synthetic Tokens@ +=
Root;
@Antlr Parse Rules@ +=
statement : funcall | assign | whileloop | ifelse;
eval : ((statement | fundef) SC)+ -> ^(Root fundef* ^(Definition ^(Name Label["main"]) ^(Parameters) ^(Body statement*)));
~~~~

The language is Turing-complete, with two C-like control-flow structures. If-statements have an optional else-clause, and both structures use braces to group their statements. Any expression that evaluates to a non-zero number is considered 'true'.

~~~~
@Antlr Tokens@ +=
LB : '{'; RB : '}'; 
If : 'if'; Else : 'else'; While : 'while';
Conditional : '<' | '>' | '\u2261' | '\u2260' | '\u2264' | '\u2265';
@Synthetic Tokens@ +=
Test; IfTrue; IfFalse; Body;
@Antlr Parse Rules@ +=
iftrue : (statement SC)*; iffalse : (statement SC)*;
ifelse : If expression LB iftrue RB (Else LB iffalse RB)? 
  -> ^(If ^(Test expression) ^(IfTrue iftrue) ^(IfFalse iffalse)?);
whileloop : While expression LB (statement SC)* RB
  -> ^(While ^(Test expression) ^(Body statement*));
~~~~

The langauge is also [untyped](http://en.wikipedia.org/wiki/Programming_language#Typed_versus_untyped_languages) - all values are 64-bit integers (it doesn't expose the x86's [floating point](http://en.wikibooks.org/wiki/X86_Assembly/Floating_Point) registers or operations), and can represent values, memory addresses or function pointers. This makes it easy to add a [foreign function interface](http://en.wikipedia.org/wiki/Foreign_function_interface) (FFI). The assembly code the compiler generates conforms to the [System V AMD64 ABI](http://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_calling_conventions) (not the Microsoft x64 calling convention that Windows uses). We'll go into this in much more detail later.

~~~~
@Antlr Tokens@ +=
LP : '('; RP : ')'; C : ','; Definition : 'fn';
@Synthetic Tokens@ += 
Parameters; Call; Name;
@Antlr Parse Rules@ +=
funcall : callable LP (expression (C expression)*)? RP -> ^(Call callable ^(Parameters expression*));
fundef : Definition Label? LP (Variable (C Variable)*)? RP LB (statement SC)* RB 
  -> ^(Definition ^(Name Label) ^(Parameters Variable*) ^(Body statement*));
~~~~

As the language doesn't specify whether numbers are signed or unsigned, all literals are specified in [hex](http://en.wikipedia.org/wiki/Hexadecimal). These literals are [limited](http://stackoverflow.com/questions/2409628/in-antlr-how-do-you-specify-a-specific-number-of-repetitions
) to at most 64 bits.

~~~~
@Antlr Tokens@ +=
Number @init{int n=0;} : '0x' (('0'..'9' | 'A' .. 'F') {++n;})+ {n > 0 && n <= 16}?;
~~~~

We'll also let programmes specify which symbols (both functions and global variables) they want to _export_. Other programmes can only linked (either statically or [dynamically](http://www.tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html)) against external symbols. You can see what symbols a library or executable contains (and exports) using GNU [nm](http://sourceware.org/binutils/docs-2.16/binutils/nm.html), e.g.:

<pre>
$ nm -r /usr/lib/libc.dylib
  U dyld_stub_binder
  U _xpc_atfork_prepare
  U _xpc_atfork_parent
  U _xpc_atfork_child
  U _write
  U _unlink
  U _time
  ...
</pre>

As a stylistic point, we'll force global (exported) functions and variables to be upper case, and local (not-exported) functions and variables to be lower case. However, we can't place restrictions on the names of foreign functions, so we'll prefix them with a '#'. We'll also force locally-declared variables to begin with a <tt>$</tt> to disambiguate calls to unexported functions from calls to function pointers stored in variables. An example: say our code has a local (non-exported) function called <tt>my_func</tt> and a function pointer to a different function stored in a local variable called <tt>my_func</tt>. If we try to compile an expression <tt>my_func();</tt> then it's not obvious which of the two functions we should call - and so we'd have to define scoping rules for our language. 

~~~~
@Antlr Tokens@ +=
FFI : '#' ('a'..'z'|'A'..'Z'|'0'..'9'|'_')+;
fragment Unexported : '_'* ('a'..'z') ('a'..'z'|'_'|'0'..'9')*;
fragment Exported : '_'* ('A'..'Z') ('A'..'Z'|'_'|'0'..'9')*;
Variable : '$' Unexported;
Label : Unexported | Exported;
~~~~

The final language feature I'll introduce are "intrinsic" functions, which are built-in functions that compile down to a single Assembly instruction. As this is only a toy language I'll just include intrinsics for some unsigned integer arithmetic operations, like <tt>+</tt> for the [ADD](http://en.wikipedia.org/wiki/X86_instruction_listings) instruction.

~~~~
@Antlr Tokens@ +=
Intrinsic : '+' | '-' | '&' | '|' | '\u00ab' | '\u00bb' ;
~~~~

Memory de-refencing and [Stack](http://en.wikipedia.org/wiki/Stack-based_memory_allocation)-based memory allocation are similar to intrinsics; they are represented by symbols that are 'called' like any other function. We'll go into the layout of the stack (and where stack-based allocation fits into this) in much more detail later.

~~~~
@Antlr Tokens@ +=
Deref : '@'; StackAlloc : '$';
~~~~

The remainder of the grammar arranges these components in a pretty straight-forward imperative style.

~~~~
@Antlr Tokens@ +=
Assign : '=';
@Antlr Parse Rules@ +=
expression : Variable | Label | Number | funcall | FFI;
assignable : Variable | Label | Deref assignable -> ^(Deref assignable) | FFI;
assign : assignable Assign expression -> ^(Assign assignable expression);
~~~~

As you can see, our langauge doesn't have return statements - instead it uses a function-call style syntax (e.g. <tt>return(a);</tt>). As we'll see below, the x86-64 ABI allows up to two integer return values from a function, and the compiler will be simpler if we don't make returns a special case.

The final list of tokens we can call like a function is therefore:

~~~~
@Antlr Parse Rules@ +=
callable : Variable | Label | Intrinsic | Conditional | Deref | StackAlloc | FFI;
~~~~

Whitespace doesn't affect how the AST is built.

~~~~
@Antlr Parse Rules@ +=
WS : (' ' | '\t' | '\r' | '\n') {$channel=HIDDEN;};
~~~~

As an example of the langauge, a function to calculate the n'th Fibonacci number (taken from the compiler's [integration tests](https://github.com/remis-thoughts/blog/tree/master/compile-to-asm/src/test/resources/test-code)) is below. Note that this implementation will always terminate as it treats its input argument as an unsigned integer.

<pre>
fn fibs($nth) {
  $last = 0x1;
  $this = 0x1;  
  while >($nth, 0x0) {
    $nth = -($nth, 0x1);
    $next = +($this, $last);
    $last = $this;
    $this = $next;
  };  
  return($this);
};
</pre>

### Code Structure ###

The outline of the Java class the compiler lives in is below. The code is divided into two main sections; the first transforms each function in the antlr AST into an array of Assembly instructions, however these instructions still refer to the variable names in the source code. The second section uses [linear programming](http://en.wikipedia.org/wiki/Linear_programming) to assign the variables referenced in each instruction to a register. This assignment must preserve the values of every variable, and if there aren't any registers available to store a value in, that value should be preserved on the stack. 

~~~~
@com/blogspot/remisthoughts/compiletoasm/Compiler.java:*@ +=
package com.blogspot.remisthoughts.compiletoasm;
@Imports@
public final class Compiler {
  public static void compile(InputStream srcIn, Appendable asmOut) throws Exception {
    @Build The AST@ 
    ProgramState program = new ProgramState();
    @First Section@
    @Write The Output@
  }
  @Static Classes@
  @Other Helpers@
}
~~~~

We're compiling for an x86-64 architecture with 8-byte pointers; we'll use this constant frequently.

~~~~
@Other Helpers@ +=
private static final long SIZE = 8;
~~~~

## (I) AST To Assembly Instructions ##

The first pass: turning an AST into pseudo-assembly, with variables, not registers. 

Each function should be written into its own <tt>List</tt> of <tt>Instruction<tt>s, and these will all end up in the [text](http://en.wikipedia.org/wiki/Code_segment) segment of the outputted Assembly code. The segment (and some other constants) [determine the permission](http://ftp.linux.org.uk/pub/linux/alpha/alpha/asm10.html#perm_tbl) of the memory that the code is loaded into at runtime.

~~~~
@First Section@ +=
for(Tree child : children(ast)) {
  parseDefinition(child, program);
}
~~~~

### The Object Model ###

We'll use subclasses of an <tt>Instruction</tt> class to represent the subset of Assembly instructions the compiler will generate. The (flat) inheritance hierarchyis shown below (via [asciiflow](http://www.asciiflow.com/#)):

<pre>
                   +-----------+
                   |Instruction|
                   +-----+-----+
                         |
     +---------+---------+---------+------+----+
     |         |         |         |      |    |
     |         |         |         |      |    |
 +---v----+ +--v-+ +-----v----+ +--v-+ +--v-+  |
 |BinaryOp| |Call| |Definition| |Jump| |Move|  |
 +--------+ +----+ +----------+ +----+ +----+  |
                                               |
   +------+------+------+------+------+--------+
   |      |      |      |      |      |
   |      |      |      |      |      |
 +-v-+ +--v-+ +--v-+ +--v--+ +-v-+ +--v-+
 |Pop| |Push| |Swap| |align| |Ret| |Head|
 +---+ +----+ +----+ +-----+ +---+ +----+
</pre>

We'll leave <tt>Instruction</tt>'s definition mostly empty for now - we'll introduce its members when we need them.

~~~~
@Static Classes@ +=
abstract static class Instruction { @Instruction Members@ }
~~~~

The "intrinsic" function calls mentioned above will map directly to instances of the <tt>BinaryOp</tt> class. While not all [x86 instructions](http://ref.x86asm.net/coder64.html) take two arguments, all the "intrinsics" this compiler supports take two, and they all modify the second operand. We'll add a <tt>q</tt> [suffix](http://en.wikibooks.org/wiki/X86_Assembly/GAS_Syntax#Operation_Suffixes) to all the intrinstic instructions as all our operands are eight bytes ("quads").

~~~~
@Static Classes@ +=
enum Op { 
  addq("+") { @ADDQ@ }, 
  subq("-") { @SUBQ@ },
  andq("&") { @ANDQ@ },
  orq("|")  { @ORQ@  },
  cmpq("?") { @CMPQ@ },
  shlq("«") { @SHLQ@ },
  shrq("»") { @SHRQ@ };
  @Op Members@
};
static final class BinaryOp extends Instruction {
  final Value arg1; final StorableValue arg2; final Op op;
  BinaryOp(Op op, Value arg1, StorableValue arg2) { this.op = op; this.arg1 = arg1; this.arg2 = arg2; }
  public String toString() { @BinaryOp x86 Code@ }
  @BinaryOp Members@
}
~~~~

 The UTF-8 string passed to the constructor of each enum constant corresponds to the list of UTF-8 strings that are valid <tt>Intrinsic</tt> tokens in the antlr grammar. A helper method in the <tt>Op</tt> enum does a lookup against this mapping in O(number of intrinsics) time. If the compiler is modified to support more instrinsic operations we should probably change this to a O(1) hash-based lookup. 

~~~~
@Op Members@ +=
final String c;
Op(String c) { this.c = c; }
private static Op parse(String c) { 
  for(Op op : values()) if(op.c.equals(c)) return op;
  throw new IllegalArgumentException();  
}
~~~~

There's also a class hierarchy to represent the different types of operand. In the final output, operands can be literals ("immediates"), registers or memory locations pointed to by a register (["register indirect addressing"](http://en.wikipedia.org/wiki/Addressing_mode#Register_indirect)). However, [at most](http://cs.smith.edu/~thiebaut/ArtOfAssembly/CH04/CH04-3.html#HEADING3-113) one operand in a BinaryOp can be indirect (i.e. a value in memory). We also want to generate [position-independent](http://en.wikipedia.org/wiki/Position-independent_code) Assembly code, so we don't want to generate absolute memory addresses.

We'll enforce the distinction between literals and registers using Java's type system. All operands are <tt>Value</tt>s, but operands that can be modified are subtypes of <tt>StorableValue</tt>. The class below is called <tt>Immediate</tt> as that's what a literal in an assembly instruction is called. 

~~~~
@Static Classes@ +=
interface Value {}
interface StorableValue extends Value {}
static final class Immediate implements Value {
  final Supplier<Long> value;
  Immediate(long value) { this.value = Suppliers.ofInstance(value); }
  Immediate(Supplier<Long> value) { this.value = value; }
  public boolean equals(Object o) { return o instanceof Immediate && ((Immediate)o).value.get() == value.get();}
  public String toString() { @Immediate x86 Code@ }
}
~~~~

As we are compiling for a single architecture (x86-64) we can "hard-code" the register names in an enum. The language we're compiling doesn't support floating-point values, so we don't need to include the [eight xmm registers](http://en.wikipedia.org/wiki/Streaming_SIMD_Extensions#Registers) added by the SSE instruction set. This means we can't call other functions that expect floating point values as parameters. The enum also has a dummy value, <tt>THESTACK</tt>. We'll use this in the second section of the article when assigning variables to registers. If we have more variables that available registers we'll store some of them on the stack. The algorithm for deciding which variable goes where will assign variables to the <tt>THESTACK</tt> register to indicate this.

~~~~
@Static Classes@ +=
enum Register implements StorableValue {
  rax, rbx, rcx, rdx, rsp /*stack pointer*/, 
  rbp /*frame pointer*/, rsi, rdi, r8, r9, 
  r10, r11, r12, r13, r14, r15, THESTACK,
  rip /*instruction pointer*/;
  public String toString() { return String.format("%%%s", name()); }
}
~~~~

The remaining <tt>Value</tt> types look like this:

<pre>
                                +-----+
                                |Value|
                                +--+--+
                                   |
                           +-------+-------+
                           |               |
                     +-----v-------+  +----v----+
                     |StorableValue|  |Immediate|
                     +-----+-------+  +---------+
                           |
                  +--------+-+--------+----------+
                  |          |        |          |
              +---v-----+ +--v--+ +---v----+ +---v----+
              |AtAddress| |Label| |Register| |Variable|
              +---------+ +-----+ +--------+ +--------+
</pre>

<tt>Variable</tt> is the only class here that won't appear in the final output; the second section of this article explains how we replace all the <tt>Variable</tt> instances with <tt>Register</tt> or <tt>AtAddress</tt> instances. We'll make a <tt>Variable</tt> a typedef of an int.

~~~~
@Static Classes@ +=
static final class Variable implements StorableValue, Comparable<Variable> {
  final int id;
  Variable(int id) { this.id = id; }
  public boolean equals(Object o) { return o instanceof Variable && ((Variable)o).id == id; }
  public int hashCode() { return id; }
  public int compareTo(Variable o) { return Ints.compare(id, o.id); }
}
~~~~

<tt>AtAddress</tt> [decorates](http://en.wikipedia.org/wiki/Decorator_pattern) a register (or <tt>Variable</tt>), using it to specify what memory address to look up. [Base plus offset](http://en.wikipedia.org/wiki/Addressing_mode#Base_plus_offset.2C_and_variations) addressing is a generalisation of register indirect addressing; the description includes an literal value to add to the memory address in the register before dereferencing it. For example, <tt>mov 0x4(%rax), %rbx</tt> where register <tt>rax</tt> contains 0x7fff5fbff9b8 means take the memory address in register <tt>rax</tt> (0x7fff5fbff9b8), add four to it (0x7fff5fbff9bc), then read 8 bytes from memory starting at that address. These 8 bytes will be in [little endian](http://en.wikipedia.org/wiki/Endianness) format on an x86-64 processor, so reverse the sequence of bytes then combine them to get a 64-bit integer, and store that integer in register <tt>rbx</tt>.

~~~~
@Static Classes@ +=
static class AtAddress implements StorableValue, Resolvable {
  final StorableValue at; final Supplier<Long> offset;
  AtAddress(StorableValue at, Supplier<Long> offset) {
    this.at = at; this.offset = offset;
  }
  AtAddress(StorableValue at, long offset) {
    this(at, Suppliers.ofInstance(offset));
  }
  public String toString() { @At Address x86 Code@ }
  public boolean equals(Object o) {
    if(!(o instanceof AtAddress)) return false;
    AtAddress other = (AtAddress) o;
    return at.equals(other.at) && offset.equals(other.offset);
  }
  public int hashCode() { return at.hashCode(); }
  @At Address Members@
}
~~~~

<tt>Label</tt>s are named locations in memory. Some of the labels in the Assembly code the compiler generates include the start of each function (so the label will point at the text segment of the memory) and the location of any global variables defined in the source code (see later). These labels will be included as symbols in the global offset table (GOT) - where they may or may not be marked as exported - so when other programmes call functions (or access variables) in the binary the linker knows where to jump to (or read from or write to) in the binary's code - more details [here](http://eli.thegreenplace.net/2011/11/03/position-independent-code-pic-in-shared-libraries).

Like the <tt>Variable</tt> type, <tt>Label</tt> includes a no-arg constructor to generate unique labels.

~~~~
@Static Classes@ +=
static final class Label implements StorableValue, Comparable<Label> {
  final String name;
  Label(String name) { this.name = name; }
  Label() { this("_l" + uniqueness.getAndIncrement()); }
  public int compareTo(Label other) {
    return name.compareTo(other.name);
  }
  public boolean equals(Object o) { 
    return o instanceof Label && ((Label)o).name.equals(name);
  }
  public int hashCode() { return name.hashCode(); }
  public String toString() { @Label x86 Code@ }
  public boolean isUpperCase() {
    return name.equals(name.toUpperCase());
  }
}
~~~~

We'll add an <tt>Instruction</tt> that defines a label. We'll use two different subclasses of <tt>Instruction</tt> to represent the two ways our generated assembly code uses label definitions. The first, <tt>Definition</tt>, is just for control flow within the function. This will never be visible from outside the function, so we don't need to worry about the visibility of the symbol it's defining. 

http://www.keil.com/support/man/docs/armasm/armasm_caccjfff.htm
http://tigcc.ticalc.org/doc/gnuasm.html#SEC48L

~~~~
@Static Classes@ +=
static class Definition extends Instruction {
  final Label label;
  Definition(Label label) { 
    this.label = label;
  }
  public String toString() { @Definition x86 Code@ }
  @Definition Members@
}
~~~~~

The second <tt>Instruction</tt> class we'll use to represent the definition of a <tt>Label</tt>, <tt>Head</tt>, only occurs as the first <tt>Instruction</tt> of the function. This <tt>Instruction</tt> defines the <tt>Label</tt> with the function's name, and the location of this <tt>Label</tt> will appear in the GOT so other code that links to the binary we're generating can call it. The <tt>isExported</tt> flag is true if this symbol should be marked as exported in the GOT.

~~~~
@Static Classes@ +=
static final class Head extends Instruction {
  final boolean isExported; final Label label;
  Head(Label label, boolean isExported) { 
    this.label = label; this.isExported = isExported;
  }
  public String toString() { @Head x86 Code@ }
  @Head Members@
}
~~~~

Some instructions are conditional - they only get executed if certain [flags](http://en.wikipedia.org/wiki/FLAGS_register) in the CPU's flag register are set. These flag bits are set as side-effects when other instructions are executed. For example, if <tt>add %rax, %rbx</tt> [overflows](http://en.wikipedia.org/wiki/Arithmetic_overflow) (i.e. <tt>rax + rbx > 2^64</tt>) then the _overflow_ bit (the 11th bit) in the flags register is set. If the next instruction was <tt>jo %rcx</tt>, then as the overflow flag is set the instruction will be executed and control flow jumps to the instruction at the memory address in <tt>rcx</tt>. We'll only use a few of the conditions supported by the x86 instruction set, and we'll only use the conditions that treat the two operands being compared as _unsigned_ 64-bit integers.

~~~~
@Other Helpers@ +=
enum Condition {
  b("<"), a(">"), e("≡"), ne("≠"), be("≤"), ae("≥");
  String c; 
  Condition(String c) { this.c = c; }
  private static Condition parse(String c) { 
    for(Condition cond : values()) if(Objects.equal(cond.c, c)) return cond;
    throw new IllegalArgumentException();  
  }
  @Condition Members@
}
~~~~

The last instruction we'll introduce for now is <tt>Move</tt>. It has a <tt>cond</tt> field to (optionally) store a condition. This instruction actually has "copy" semantics; the second operand is set to the value of the first, and the first operand is left unchanged. We'll add a helper method for generating unconditional moves here too:

~~~~
@Static Classes@ +=
static final class Move extends Instruction {
  final Value from; final StorableValue to; final Condition cond;
  Move(Value from, StorableValue to, Condition cond) { 
    this.from = from; this.to = to; this.cond = cond; 
  }
  public String toString() { @Move x86 Code@ }
  @Move Members@
}
static Instruction move(Value from, StorableValue to) {
  return new Move(from, to, null);
}
~~~~

When we compile conditional (if-) statements later we could use conditional jump or conditional move instructions. The relative performance of these two choices [differ](https://mail.mozilla.org/pipermail/tamarin-devel/2008-April/000454.html) by processor implementation, so we'll arbitarily decide that our compiler will generate conditional moves.

### State Objects ###

We'll map each function independentally (remember we swept all statements in the source code that weren't in a function into the <tt>_main</tt> function in the antlr grammar).

We'll need to have the global state (which we'll keep in a single instance of <tt>ProgramState</tt>) handy, in case a function defines any global variables or otherwise needs to access the global state. We'll also need an object to hold the per-function state (an instance of <tt>ParsingState</tt>), such as the parameters and any variables it declares. The per-function state should keep a reference to the <tt>ProgramState</tt> so that we can easily construct new <tt>ParsingState</tt>s as we come across nested function definitions.

~~~~
@Other Helpers@ +=
static final class ProgramState {
  final List<ParsingState> text = Lists.newArrayList();
  final Map<Label, Immediate> globals = Maps.newTreeMap();
}
static final class ParsingState {
  final ProgramState program;
  ParsingState(Tree def, ProgramState program) {
    (this.program = program).text.add(this);
    @Get Function Name@
    @Are We Starting The Main Function@
    @Set Up Stack Allocation@
    @Identify Callee Saves Registers@
    @Parse Function Parameters@
    @Set Up LP Problem@
  }
  @Parsing State Members@
}
~~~~

### Connecting Instructions Together ###

We'll make our <tt>Instruction</tt>s form an [intrusive doubly-linked list](http://www.boost.org/doc/libs/1_35_0/doc/html/intrusive/intrusive_vs_nontrusive.html) by adding pointers to the <tt>Instruction</tt>s immediately preceding and following each <tt>Instruction</tt>. These will be null for the first <tt>Instruction</tt> (the function's <tt>Definition</tt>) and for each <tt>Return</tt> (i.e. each exit from the function). This will give us an O(1) insertion into to the middle of the list, and (unlike linked-list containers, like Java's <tt>LinkedList</tt>) will allow us to link one <tt>Instruction</tt> to override the default implementation to allow multiple predecessors or successors, which will be useful when we come to (conditional) <tt>Jump</tt> <tt>Instruction</tt>s. We'll also make this <tt>append</tt> return the newly-added <tt>Instruction</tt> so you can use it in a [fluent](???) way.

~~~~
@Instruction Members@ +=
Instruction next, prev;
Instruction append(Instruction i, ParsingState state) {
  if(next != null) {
    i.next = next;
    next.prev = i;
  }
  next = i;
  i.prev = this;
  @After Append@
  return i;
}
~~~~

We'll keep references to the first <tt>Instruction</tt> as a member of the <tt>ParsingState</tt>, so we can easily iterate forwards through the function's code. Our compilation will occur in a series of "passes", or iterations through the <tt>Instruction</tt>s in order. We'll try to do as much work in each pass as possible (not least because iterating through the <tt>Instruction</tt>s will not be very cache-friendly).

~~~~
@Parsing State Members@ +=
final Instruction head;
~~~~

At this point we have two choices for the semantics of <tt>next</tt> and <tt>prev</tt>: (1) they could refer to the order the <tt>Instruction</tt>s appear in the final output or (2) they could refer they the order <tt>Instruction</tt>s are visited during execution (i.e. they match control flow). This has many interesting consequences; (1) makes iterating through the code visiting each <tt>Instruction</tt> exactly once much easier, but (2) gives us dead code elimination for free, as we'll see with the <tt>Ret</tt> <tt>Instruction</tt>. For this reason we'll chose the semantics of (2).

The <tt>Ret</tt> <tt>Instruction</tt> (an x86 <tt>ret</tt>) is a good example of non-standard <tt>Instruction</tt> linking. We'll give ourselves dead-code elimination for free by simply not appending any <tt>Instruction</tt>s to a <tt>Ret</tt>. <tt>Ret</tt>'s <tt>append</tt> returns the <tt>Ret</tt>  itself, indicating it's still the end of the linked list. We'll also clear the appended <tt>Instruction</tt>'s <tt>prev</tt> pointer to keep its state consistent, as the <tt>Ret</tt>'s <tt>next</tt> pointer will never be set to a non-null value.

~~~~
@Static Classes@ +=
static final class Ret extends Instruction {
  @Ret Members@
  public String toString() { return @Ret x86 Code@; }
  Instruction append(Instruction i, ParsingState state) {
    i.prev = null;
    return this;
  }
};
~~~~

This behaviour means the linked list starting at the <tt>head</tt> element in <tt>ParsingState</tt> is only a list (a chain of <tt>Instruction</tt>s with one predecessor and one successor) in the most simple functions. In most cases - functions with more than one exit point, or <tt>Return</tt> statement, we'll end up with a tree of <tt>Instruction</tt>s starting at the <tt>head</tt>, with each branch terminating in a <tt>Ret</tt> and where each fork (or loop) in the tree comes from a <tt>Jump</tt> (conditional or otherwise).

We'll support removing <tt>Instruction</tt>s from a function too; we'll cleanly remove them from the intrinsic linked list, so if they have a <tt>next</tt> <tt>Instruction</tt> then that will become the <tt>Instruction</tt>'s <tt>prev</tt> <tt>Instruction</tt>'s <tt>next</tt>. We'll make <tt>delete</tt> return the previous <tt>Instruction</tt> to give this method a [fluent interface](https://en.wikipedia.org/wiki/Fluent_interface) - which should make code that deletes a series of <tt>Instruction</tt>s more readable.

~~~~
@Instruction Members@ +=
Instruction delete(ParsingState state) {
  Instruction oldPrev = prev;
  if(prev != null)
    prev.next = next;
  if(next != null)
    next.prev = prev;
  prev = next = null;
  @After Delete@
  return oldPrev;
}
~~~~

We should also define here the exact semantics of <tt>append</tt>, and in particular when it should be called. When we're generating <tt>Instruction</tt>s that follow linear control flow (i.e. don't have jumps) then we know <tt>append</tt> will be called exactly once on each new <tt>Instruction</tt>, even if this call doesn't actually wire up the new <tt>Instruction</tt> (e.g. the implementation in <tt>Ret</tt>). To keep the code simple, for non-linear control flow (e.g. when <tt>append</tt>ing a <tt>Definition</tt> that can be jumped to or reached directly from the previous <tt>Instruction</tt>) we'll enforce the condition that <tt>append</tt> will *still only be called once* on the <tt>Instruction</tt> being appended, regardless of the number of ways control flow can reach that <tt>Instruction</tt>. A corollery of this is that wiring up jump <tt>Instruction</tt>s to their corresponding <tt>Definition</tt> <tt>Instruction</tt>s must occur via some other mechanism, which we'll cover later.

Another clarification we should make should be what initialization work we do in an <tt>Instruction</tt>'s constructor and what initialization work we do elsewhere. This question is very relevant for [incremental compilers](https://en.wikipedia.org/wiki/Incremental_compiler); compilers that modify their outputs as the source code they're given is modified. While this article doesn't cover building an incremental compiler, we want to make that an easy extension to implement. In particular, as parts of the source code get added, deleted or moved the corresponding parts of the Antlr-generated AST should be updated, which would then add, remove and (importantly) move sequences of <tt>Instruction</tt>s in the linked list. Adding and deleting are covered by the <tt>append</tt> and <tt>delete</tt> methods detailed above, but some forward planning is needed if we're to support detaching a chain of <tt>Instruction</tt>s and <tt>append</tt>ing it elsewhere (much more efficient than deleting the chain and creating a new one just like it elsewhere).

### Visiting Instructions ###

At the minute, each <tt>Instruction</tt> has at most one successor and one predecessor (via its <tt>next</tt> and <tt>prev</tt> links). We'll allow subclasses to extend this, by ensuring that when we're traversing the <tt>Instruction</tt> list we only access those members via these <tt>next()</tt> and <tt>prev</tt> method that subclasses can override:

~~~~
@Instruction Members@ +=
Set<Instruction> prev() {
  return prev == null ?
    Collections.<Instruction>emptySet() :
    ImmutableSet.of(prev);
}
Set<Instruction> next() {
  return next == null ?
    Collections.<Instruction>emptySet() :
    ImmutableSet.of(next);
}
~~~~

### Recursing Down Into The AST ###

A typical function definition AST that the lexer and parser gives us looks like this:

<pre>
                                                                    +--------+
                                                                    |Variable|
                                                                    |   "c"  |
                                                                    +--------+
                                                                         ^
                                                       +--------+  +----------+
                                +--------+ +--------+  | Return |  |Parameters|
                                |Variable| |Variable|  +--------+  +----------+
                                |  "c"   | |  "b"   |     ^              ^
                                +--------+ +--------+     +--------------+
                                       ^        ^          ^
                                       +---+----+          |
+-------------+ +--------+ +--------+  +---+--+        +---+--+
|    Label    | |Variable| |Variable|  |Assign|        | Call |
|"hello_world"| |  "a"   | |  "b"   |  +------+        +------+
+-------------+ +--------+ +--------+     ^                ^
       ^             ^       ^            +--------+-------+
       |             +---+---+                     |
     +-+--+        +-----+----+                 +--+-+
     |Name|        |Parameters|                 |Body|
     +----+        +----------+                 +----+
       ^               ^                           ^
       +---------------+---------------------------+
                 +-----+----+
                 |Definition|
                 +----------+  
</pre>

We're going to be doing a lot of tree navigation, so here's some helper methods to access a tree node's type (e.g. Body or Return), a tree node's text (e.g. "hello_world" in the example above), to access nodes along a given path and to get all the direct children of a tree node along a given path:

~~~~
@Other Helpers@ +=
static Tree get(Tree from, int... indices){
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

For example, <tt>children(&lt;the definition node&gt;, 2)</tt> would return a list containing the "Assign" and "Return" nodes, and <tt>get(&lt;the body node&gt;, 0, 1)</tt> would return the Variable "b" node.

It turns out that we can parse each statement of a function independently. However, as a return statement is optional in the language we'll add a <tt>ret</tt> instruction to the generated Assembly code if there's not one. As we'll see later, this will return whatever's in the <tt>rax</tt> register as the return value of the function. We'll return the function's symbol as a <tt>Label</tt> in case the calling code needs to assign it to a variable, but doesn't know what the function's name will be (e.g. if this function is anonymous).

~~~~
@Other Helpers@ +=
static Label parseDefinition(Tree def, ProgramState program) {
  ParsingState state = new ParsingState(def, program);  
  state.parseStatements(children(def, 2));
  if(!(state.current instanceof Ret)) {
    state.addReturn(0);
  }
  @Do Register Allocation@
  return state.head.label;
}
~~~~

We'll now talk about how we'll build up the intrinsically linked list of <tt>Instruction</tt>s. We'll keep the 'current' <tt>Instruction</tt> in a the <tt>ParsingState</tt> so it's easy to access from helper methods in <tt>ParsingState</tt> (i.e. it doesn't have to be passed as a parameter and returned), and each time we call <tt>append</tt> to add a new <tt>Instruction</tt>, we'll set <tt>current</tt> to the return value of <tt>append</tt> (which will usually be in <tt>Instruction</tt> just added).

~~~~
@Parsing State Members@ +=
Instruction current = null;
public void append(Instruction i) {
  current = current.append(i, this);
}
~~~~

The first instruction we need to generate is the Assembly label that marks the start of our function in the generated code. We'll add a leading underscore to the variable's name as it appears in the source code to comply with the [Linux & OSX ABI](http://stackoverflow.com/questions/2627511/why-do-c-compilers-prepend-underscores-to-external-names). If the function is anonymous (there's no name in the source code) we'll generate a random one (using the <tt>new Label()</tt> constructor) and ensure that that function isn't exported.

~~~~
@Get Function Name@ +=
current = head = new Head(
  hasChildren(def, 0) ? new Label("_" + text(def, 0, 0)) : new Label(), 
  isMainFn() || myName.isUpperCase());
~~~~

We'll assign a unique number to each <tt>Variable</tt>. This way we can keep track of how many distinct <tt>Variable</tt>s we've got, and it'll be useful later for generating array indices for a <tt>Variable</tt>. We'll turn the <tt>ParsingState</tt> into a factory, and use the convention that passing <tt>null</tt> as the parameter returns a new unique <tt>Variable</tt>.

~~~~
@Parsing State Members@ +=
Map<String, Variable> variables = Maps.newTreeMap();
int numVariables = 0;
StoreableValue variable(String name) {
  if(name == null) return new Variable(numVariables++); // anonymous
  @Is This A Register Name@
  @Is This A Parameter@
  Variable ret = variables.get(name);
  if(ret == null)
    variables.put(name, ret = new Variable(numVariables++));
  return ret;
}
~~~~

If the AST node is a <tt>Variable</tt> it could be a reference to a local variable defined in this function or a reference to one of the parameters (as they are AST nodes of type <tt>Variable</tt> too). We've seen that one of the <tt>ParsingState</tt>'s fields is a <tt>Map</tt> of <tt>Variable</tt> names to <tt>Value</tt>s that access those parameters (but not how that map's built up yet). We'll look up our new <tt>Variable</tt> in this map to see if it's actually a reference to one of the function parameter - and if so we'll return the <tt>StorableValue</tt> that accesses it instead.

~~~~
@Is This A Parameter@ +=
if(params.containsKey(name))
  return params.get(name);
~~~~

We'll take this opportunity to give the programming language more control over register access: if a <tt>Variable</tt> has the name of a <tt>Register</tt>, we'll treat that as a direct access of that register. While you could use this feature as a way of overriding the register allocation algorithm in the second section, this feature is intended to provide a way of inspecting the contents of 'special' registers like the stack pointer (<tt>rsp</tt>) or the instruction pointer (<tt>rip</tt>):

~~~~
@Is This A Register Name@ +=
Optional<Register> reg = Enums.getIfPresent(Register.class, name);
if(reg.isPresent())
  return ret.get();
~~~~

What we do for each statement depends on the statement's type, so we'll switch on that:

~~~~
@Parsing State Members@ +=
private void parseStatements(Iterable<Tree> statements) {
  for (Tree statement : statements) {
    switch (type(statement)) {
      case Call :   call(statement); break;
      case Assign : @Parse An Assign Statement@ break;
      case While :  @Parse A While Statement@ break;
      case If :     @Parse An If Statement@ break;
    }
  }
}
~~~~

### Processing Expressions ###

Before we get on to statements we'll start by parsing expressions, which we do in<tt>getAsValue</tt>. In a similar way to <tt>parseStatements</tt> we'll use a switch statement to process each type of expression independently. As expressions can contain other expressions (such as in the example AST of <tt>my_func(MY_GLOBAL,@(my_address))</tt> below) this function is often called recursively to flatten the tree of expressions into a list of Assembly instructions.

<pre>
                    +------------+
                    |  Variable  |
                    |"my_address"|
                    +------------+
                           ^
        +-----------+   +--+--+
        |  Label    |   |Deref|
        |"MY_GLOBAL"|   +-----+
        +-----------+      ^
               ^           |
               +-----+-----+
+---------+    +-----+----+
|Variable |    |Parameters|
|"my_func"|    +----------+
+---------+          ^
    ^                |
    +------+---------+
         +-+--+
         |Call|
         +----+
</pre>

The switch statement - note that for a <tt>Definition</tt> node the <tt>Value</tt> returned is the <tt>Label</tt> containing the newly-defined function's symbol. As each function appends instructions to its own <tt>List</tt> of instructions in its <tt>ParsingState</tt>, and these <tt>List</tt>s are appended to the <tt>List</tt> of <tt>List</tt>s in the <tt>ProgramState</tt> the functions appear in generated Assembly code in the order they are defined. The separate <tt>List</tt>s of instructions ensure that no two functions' instructions are interleaved.

~~~~
@Parsing State Members@ += 
private Value getAsValue(Tree t) {
      switch (t.getType()) {
        case Definition:
            return parseDefinition(t, program);
        case Label:
            { @Get A Label@ }
        case Variable:
            { @Get A Variable@ }
        case FFI:
            { @Get A FFI@ }
        case Call:
            { @Get A Call@ }
        case Number :
            { @Get A Literal@ }
        default :
            throw new IllegalArgumentException();
    }
}
~~~~

Sometimes we'll need to ensure the <tt>Value</tt> is a <tt>StorableValue</tt> as the <tt>BinaryOp</tt> instructions can't have two literals or two memory addresses as operands. If we see this we'll move one of the literals or memory addresses to a new (unique) <tt>Variable</tt> using the code below, and we'll use that <tt>Variable</tt> as one of the original instruction's operands.

~~~~
@Parsing State Members@ +=
private StorableValue toStorable(Value v) {
  if (StorableValue.class.isAssignableFrom(v.getClass())) 
    return (StorableValue) v;
  Variable var = variable(null);
  append(move(v, var));
  return var;
}
~~~~

We've done all the work of deciding whether a string in the source code refers to a <tt>Variable</tt> or a parameter in the <tt>variable(..)</tt> function above, so we can just re-use that logic here:

~~~~
@Get A Variable@ +=
return variable(text(t).substring(1));
~~~~

If the expression is a reference to a symbol defined elsewhere, we'll just extract the symbol name from the AST node's text by stripping off the leading "#". We'll leave this label our generated Assembly code, and when it's assembled the assembler will add it to the GOT as an undefined symbol. When the assembled code is linked the (static or dynamic) linker will look for a definition of this symbol elsewhere, and will fail if it can't find a shared library, executable or one of the other files it's linking that exports the symbol.

~~~~
@Get A FFI@ +=
return new Label(text(t).substring(1));
~~~~

<tt>Label</tt>s are symbols like the (FFI) symbols above, however we have to put them in the generated code's [data segment](http://en.wikipedia.org/wiki/Data_segment). We'll add a leading <tt>_</tt> to the name in the source code as we did when generating the function's name, and we'll also make sure the variables are initialized to zero. This means that the assembler will write zeros to the data segment of the binary it outputs, and at runtime the [loader (not the linker)](http://www.linuxjournal.com/article/6463) copies these zeros into the addresses of the process that the loader has assigned the corresponding symbols to.

~~~~
@Get A Label@ +=
return new Label("_" + text(t));
~~~~

Literals expressions are easy to evaluate. We'll strip off the leading "0x" string that appears in the source code (it's just for clarity) as Java's <tt>Long.parseLong</tt> only expects digits from the numeric base in the second argument (base 16 means it only wants digits that are 0-9 and A-E). 

~~~~
@Other Helpers@ +=
static long parseHex(Tree t) {
  return Long.parseLong(t.getText().substring(2), 16);
}
@Get A Literal@ +=
return new Immediate(parseHex(t));
~~~~

The remaining case in <tt>getAsValue</tt> deals with <tt>Call</tt> nodes in the AST. In our source language C-like call syntax is used for several things besides function calls, so we'll need another switch statement to disambiguate:

~~~~
@Get A Call@ +=
switch(type(t, 0)) {
  case Label:
  case Variable:
  case FFI:
    if("return".equals(text(t, 0)))
      { @Parse A Return@ }
    else
      { @Parse A Function Call@ }
  case Intrinsic: 
    { @Parse An Intrinsic Call@ }
  case Conditional:
    { @Parse A Conditional Call@ }
  case Deref:
    { @Parse A Dereference@ }
  case StackAlloc:
    { @Parse A Stack Allocation@ }
}
~~~~

I said above that "intrinsics", or UTF-8 symbols in the source code corresponded to exactly one Assembly instruction. However, the semantics of the langauge we're compiling don't match up with the x86 semantics of these instructions. When we compile <tt>c = +(a, b)</tt> we don't want to generate Assembly code that modifies <tt>b</tt>, so we can't output <tt>add a, b; mov b, c</tt>. Instead we'll create a temporary variable and copy one of the operands into it, so when we perform the desired <tt>Op</tt> on it we no longer modify the original operands as side-effects; for <tt>c = +(a, b)</tt> we'd generate <tt>mov b, c; add a, c</tt> (remembering that an x86 <tt>mov</tt> has copy semantics).

~~~~
@Parse An Intrinsic Call@ +=
Value arg1 = getAsValue(get(t, 1, 0));
Value arg2 = toStorable(getAsValue(get(t, 1, 1)));
Variable ret = variable(null);
append(move(arg1, ret));
append(new BinaryOp(Op.parse(text(t, 0)), arg2, ret));
return ret;
~~~~

The various <tt>Condition</tt> tokens in the langauge's grammar (e.g. <, ≠) can be used as boolean functions - functions that return <tt>0x1</tt> for true and <tt>0x0</tt> for false. The [CMP](http://ref.x86asm.net/coder64.html#x38) x86 instruction subtracts the second operand from the first (using signed arithmetic) and sets the overflow, sign, zero, adjust, parity and carry flags based on the results (there's a longer description in [Intel's architecture manual](http://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-manual-325462.pdf) and a good overview [here](https://www.hellboundhackers.org/articles/729-jumps-flags-and-the-cmp-instruction.html)). The conditional <tt>mov</tt> instruction reads these flags, and is only executed if the [necessary flags](http://x86.renejeschke.de/html/file_module_x86_id_34.html) are set. We'll therefore set the result to zero, and conditionally move a one into it:

~~~~
@Parse A Conditional Call@ +=
Condition cond = Condition.parse(text(t, 0));
Value arg1 = getAsValue(get(t, 1, 1));
StorableValue arg2 = toStorable(getAsValue(get(t, 1, 0)));
append(new BinaryOp(Op.cmpq, arg1, arg2));
Variable ret = variable(null);
append(move(new Immediate(0), ret));
append(new Move(new Immediate(1), ret, cond));
return ret;
~~~~

A <tt>Dereference</tt> in the language is another "intrinsic". It means we should use the argument as a memory address, and should return the value at that address. We'll have to take a bit of care dealing with nested dereferences (e.g. <tt>@(@(my_address))</tt>) as x86 instructions only support a single dereference (e.g. <tt>mov (%rax), %rax</tt> is valid, but <tt>mov ((%rax)), %rax</tt> isn't). In this case we'll insert an intermediate variable, so if we're dereferencing a <tt>Value</tt> that is already an indirect address we'll load the second memory address from memory into a register, and use it to load the actual value (so <tt>@(@(my_address))</tt> becomes <tt>mov (my_address), tmp; mov (tmp), the_result</tt>):

~~~~
@Parse A Dereference@ +=
Value val = getAsValue(get(t, 1, 0));
if(val instanceof StorableValue && !(val instanceof AtAddress))
  return new AtAddress((StorableValue)val, 0);
else {
  Variable ret = variable(null);
  append(move(val, ret));
  return new AtAddress(ret, 0);
} 
~~~~

### Returning From A Function ###

Returns are another "intrinsic" function call. According to [the ABI](http://www.x86-64.org/documentation/abi.pdf), functions can return up to two integer values in the <tt>rax</tt> and <tt>rdx</tt> registers. Note that this is not directly exposed in C, but some C compilers can make a function that return a struct with two integer members return that struct in these two registers.

~~~~
@Other Helpers@ +=
static final Register[] RETURNS = { rax, rdx };
~~~~

However, we'll store the exact number of <tt>Variable</tt> (0, 1 or 2) returned from the function by a particular <tt>Ret</tt> <tt>Instruction</tt> in a member, <tt>varsReturned</tt>. Note that we won't ensure that every <tt>Return</tt> statement has the same number of arguments, so different paths through the function could return different numbers of <tt>Variable</tt>s.

~~~~
@Ret Members@ +=
final int varsReturned;
Ret(int varsReturned) {
  this.varsReturned = varsReturned;
  @Ret Constructor@
}
~~~~

We'll evaluate the zero, one or two expressions that we'll return, and then add <tt>Move</tt> instructions to put the return values into the right registers, and finally call the same <tt>addReturn</tt> method we saw in <tt>parseDefinition</tt>. We'll look at <tt>getAsValue</tt> shortly - as the expression attached to the return node can be quite complicated (e.g. the return value of a function call, or the <tt>Label</tt> returned by a nested function defintion) we'll have to calculate that first. The <tt>Value</tt> that is the net result of this expression is returned by <tt>getAsValue</tt>.

~~~~
@Parse A Return@ +=
int varsReturned = Math.min(numChildren(t, 1), RETURNS.length);
Value[] returns = new Variable[varsReturned];
for(int r = 0; r < varsReturned; ++r)
  returns[r] = getAsValue(get(t, 1, r));
for(int r = 0; r < varsReturned; ++r)
  append(move(returns[r], RETURNS[r]));
addReturn(varsReturned);
~~~~

<tt>addReturn</tt> exists to ensure the Assembly code we generate for our function maintains the invariants in [ABI](http://www.x86-64.org/documentation/abi.pdf). In particular, we must ensure that the values in the following six registers when we leave the function must be the same as when our function was called.

~~~~
@Other Helpers@ +=
static final Register[] CALLEE_SAVES = { rbx, rbp, r12, r13, r14, r15 };
~~~~

We'll move the values in the registers we have to save into <tt>Variable</tt>s. These instructions are added in the constructor of the <tt>ParsingState</tt>, so will be before any instructions added by the statements we process. The variable names have an exclamation mark in them so they can't collide with variable names in the code we're compiling. We'll move the saved values back into their corresponding registers before we return from the function.

~~~~
@Identify Callee Saves Registers@ +=
for(Register r : CALLEE_SAVES)
  append(move(r, variable(CALLEE_SAVE_VAR + r)));
@Parsing State Members@ +=
void addReturn(int varsReturned) {
  for(Register r : CALLEE_SAVES)
    append(move(variable(CALLEE_SAVE_VAR + r), r));
  @Before Returning From Function@
  @Are We Leaving The Main Function@
  append(new Ret(varsReturned));
}
@Other Helpers@ +=
private static final String CALLEE_SAVE_VAR = "save!";
~~~~

### Function Calls ###

An architecture's [calling convention](http://www.agner.org/optimize/calling%5Fconventions.pdf) determines where to put a function's arguments before jumping to the first instruction of that function, where to read the return value from when control flow returns and how many bytes the stack should be aligned to. While 32-bit x86 architectures have a huge variety ([the list](http://en.wikipedia.org/wiki/X86_calling_conventions#List_of_x86_calling_conventions) includes cdecl, stdcall, fastcall & thiscall) the x86-64 landscape is a lot simpler. This compiler will output code that uses the [System V AMD ABI](http://people.freebsd.org/~obrien/amd64-elf-abi.pdf), which is used by the Linux, BSD & OSX OSes ([here's](http://www.classes.cs.uchicago.edu/archive/2011/spring/22620-1/docs/handout-03.pdf) a brief overview). The [Microsoft x86] convention (http://msdn.microsoft.com/en-us/library/9b372w95%28v=VS.80%29.aspx) only differs slightly - fewer function parameters are passed in registers - so it should be relatively simple to modify the compiler to produce Windows-compatible Assembly code.

However, both x86-64 conventions agree that 64-bit integers are returned in the <tt>rax</tt> register:

~~~~
@Parse A Function Call@ +=
call(t);
Variable var = variable(null);
append(move(Register.rax, var));
return var;
~~~~

The System V AMD ABI says that the first six integer parameters should be passed in the registers below, while the Microsoft x86 convention uses only <tt>rcx</tt>, <tt>rdx</tt>, <tt>r8</tt> and <tt>r9</tt> to pass the first four integer parameters. As noted above this compiler won't generate code that passes floating point parameters via the xmm registers.

~~~~
@Other Helpers@ +=
static final Register[] PARAMETERS = { rdi, rsi, rdx, rcx, r8, r9 };
~~~~

As the language supports function pointers <tt>Call</tt> instructions can have a <tt>Label</tt> or a <tt>Register</tt> as their operand. We'll also make the <tt>Call</tt> <tt>Instruction</tt> keep track of exactly how many parameters we pass in registers in a <tt>registerArgs</tt> member, as this will be useful when analyzing the control flow later.

~~~~
@Static Classes@ +=
static final class Call extends Instruction {
  final Value name; final int registerArgs;
  Call(Value name, int registerArgs) { 
    this.name = name; this.registerArgs = registerArgs;
  }
  public String toString() { @Call x86 Code@ }
  @Call Members@
}
@Make The Call@ +=
append(new Call(getAsValue(get(t, 0)), registerArgs));
~~~~

We'll start by using <tt>getAsValue</tt> to generate code that evaluate all the expressions we're going to pass as parameters. These expressions can be complicated (and can even include other function calls). We need to keep all the parameter <tt>Value</tt>s we've calculated so far, which reduces the registers available for evaluating subsequent parameters. While we can't avoid this for the six parameters we're passing in registers, we could calculate the parameter values we're passing on the stack and put them there - so they don't need to take up registers. However, this means we can't use the stack layout described in next, so we won't do this.

~~~~
@Parsing State Members@ +=
private void call(Tree t) {
  List<Value> values = Lists.newArrayList();
  for (Tree argument : children(t, 1))
    values.add(getAsValue(argument));
  @Ensure Stack Is Aligned@
  @Put Parameters In Registers Or On The Stack@
  @Make The Call@
  @Remove Args From Stack@
  @Function Call Bookkeeping@
}
~~~~

We'll add instructions to move the first six parameters (or all of them if there are less than six) into the registers named in the ABI (the <tt>PARAMETERS</tt> array). The values passed on the stack should be in stored reverse order, so the last parameter is furthest in to the stack, and the seventh parameter (the first parameter not stored in a register) is at the top of the stack. The <tt>storeArg</tt> method puts the parameter <tt>Value</tt> at the right index in the stack, but we'll have to cover the stack layout before we see how that works. 

We need to be reasonably careful when moving parameter <tt>Value</tt>s into the <tt>PARAMETERS</tt> register; they should be the last instructions before we execute the <tt>Call</tt> so 

~~~~
@Put Parameters In Registers Or On The Stack@ +=
int numArgs = values.size(), 
    registerArgs = Math.min(numArgs, PARAMETERS.length),
    storedArgs = numArgs - registerArgs;
for(int r = storedArgs - 1; r >= 0; --r) {
  Value arg = values.get(registerArgs + r);
  @Move Values From Memory@
  append(storeArg(arg, r));
}
for(int r = 0; r < registerArgs; ++r) // regs second so they don't get flattened by mem switch
  append(move(values.get(r), PARAMETERS[r]));
~~~~

### The Stack Layout ###

We've gone about as far as we can without explaining what the stack contains at runtime. At runtime, the stack stores memory allocated by a function call (<tt>$</tt> in our language, or [<tt>alloca</tt>](http://man7.org/linux/man-pages/man3/alloca.3.html) in C). The stack also stores the values of variables that can't be stored in registers (when there are too many; we avoid this where possible as RAM access is much slower than register access). Finally, the stack also stores the seventh and later parameter values when calling another function.

The compiler stack memory is laid out in two different ways, depending on whether we know at compile-time how much is needed. The only case where we don't know is if there are dynamic stack allocations (<tt>$</tt> calls) with arguments that aren't <tt>Immediate</tt>s. A function has to leave the stack pointer where it was at the start of the function (note that the <tt>call</tt> instruction pushes the return address on the stack, and the <tt>ret</tt> instruction removes the return address off the stack). We allocate memory on the stack by subtracting the number of bytes needed from the stack pointer (the <tt>rsp</tt> register). The <tt>rsp</tt> register always points to an allocated byte of memory. If we know how many bytes we need at compile time, we can just subtract that number from the stack pointer at the start of the function, and add it back to the stack pointer just before every <tt>ret</tt> in the function's generated code. However, if we don't know how much memory we'll need to allocate, we'll use a [frame pointer](http://en.wikipedia.org/wiki/Call_stack#The_stack_and_frame_pointers). This means we'll store the value of the stack pointer at the start of the function in a <tt>Variable</tt>, and before every <tt>ret</tt> in the function we'll copy this value back into <tt>rsp</tt>.

~~~~
@Parsing State Members@ +=
final StorableValue framePointer;
@Other Helpers@ +=
static boolean hasDynamicAllocation(Tree t) {
  if(type(t) == StackAlloc && type(t.getParent(), 1, 0) != Number) return true;
  for(Tree child : children(t))
    if(hasDynamicAllocation(child)) return true;
  return false;
}
@Set Up Stack Allocation@ +=
if(hasDynamicAllocation(get(def, 2))) {
  framePointer = variable(null);
  append(move(rsp, framePointer));
} else
  framePointer = null;
@After Stack Allocation Setup@
~~~~

The frame pointer is restored in the <tt>addReturn</tt> method, so it gets inserted before every return statement in the source code and as part of the return statement we add at the end of a function if it isn't already there.

~~~~
@Before Returning From Function@ +=
if(framePointer != null)
  append(move(framePointer, Register.rsp));
~~~~

The two methods of laying out the stack memory means we'll generate Assembly code like GCC with the [-fomit-frame-pointer](http://gcc.gnu.org/onlinedocs/gcc-3.4.4/gcc/Optimize-Options.html) option turned on.

If we know total number of stack bytes allocated, and we know the most number of stack arguments used by any function this function calls, then we only need to do a single stack allocation each function, regardless of the number of function calls this function makes. For example, if a function has eight parameters, makes a call to a function with seven parameters, makes a call to a function with eigth parameters, has one stack allocation of 16 bytes and (after the register assignment in section 2 of the article) needs to store the values of two variables on the stack at different points in the function, then the stack would look like:

<pre>
               +-----------------+
This           | 8th Parameter   |
function's     +-----------------+
parameters     | 7th Parameter   |
               +-----------------+
               | Return Address  |
               +-----------------+
               |Gap for Alignment|
               +-----------------+
This           |8-byte Variable  |
function's     +-----------------+
statically     |8-byte Variable  |
allocated      +-----------------+
stack          |    16-byte      |
variables      |    Variable     |
               |                 |
               +-----------------+
Parameters for | 8th Parameter   |
functions this +-----------------+
function calls | 7th Parameter   | &gt;--- stack pointer
               +-----------------+
</pre>

However, if a function contains dynamic stack allocation we don't know where the top of the stack will be - so we can't statically allocate space for passing parameters to function calls as we may give the space on the stack to dynamic allocations. In the example above, if the function also dynamically allocated some memory (but we don't know how much), and also calls a function with nine arguments then the stack would look like:

<pre>
               +-----------------+
This           | 8th Parameter   |
function's     +-----------------+
parameters     | 7th Parameter   |
               +-----------------+
               | Return Address  | &gt;--- frame pointer
               +-----------------+
This           |8-byte Variable  |
function's     +-----------------+
statically     |8-byte Variable  |
allocated      +-----------------+
stack          |    16-byte      |
variables      |    Variable     |
               |                 |
               +-----------------+
               |Gap for Alignment| &gt;--- stack pointer (before dynamic allocation)
               +-----------------+
               |  Dynamically    |
               |  allocated      |
               |variable, padded |
               |  to 16-byte     |
               |   alignment     | &gt;--- stack pointer (after dynamic allocation)
               +-----------------+
               |Gap for Alignment|
               +-----------------+
Parameters for | 7th Parameter   |
functions this +-----------------+
function calls | 8th Parameter   |
               +-----------------+
               | 9th Parameter   | &gt;--- stack pointer (just before function call)
               +-----------------+
</pre>

In general, the compiler tests to see if the <tt>framePointer</tt> variable is null to decide whether it's generating code for a statically-allocated or a dynmaically-allocated function. When moving <tt>Value</tt>s onto the stack to pass them as parameters to another function, a statically allocated function can just move the <tt>Value</tt> into the pre-allocated spot - otherwise we'll have to use a <tt>push</tt> instruction to decrement <tt>rsp</tt> by eight and store the <tt>Value</tt> in the new location <tt>rsp</tt> points to:

~~~~
@Parsing State Members@ +=
Instruction storeArg(Value arg, int position) {
  if(framePointer == null)
    return move(toRegister(arg), new AtAddress(rsp, position * SIZE));
  else
    return new Push(arg);
}
@Static Classes@ +=
static final class Push extends Instruction {
  final Value value;
  Push(Value value) { this.value = value; }
  public String toString() { @Push x86 Code@ }
  @Push Members@
}
~~~~

Now, if we want to generate a <tt>mov</tt> instruction store the <tt>arg</tt> <tt>Value</tt> on the stack we have to be careful to avoid generating a <tt>mov</tt> instruction where both operands are memory addresses (as this is illegal in the x86 architecture). If we do have two <tt>AtAddress</tt> operands we'll insert a <tt>mov</tt> instruction to move one into a register.

~~~~
@Parsing State Members@ += 
private Value toRegister(Value v) {
  if(v instanceof AtAddress) {
    Variable loaded = variable(null);
    append(move(v, loaded));
    v = loaded;
  }
  return v;
}
~~~~

The 8-byte gaps labelled "Gap for Alignment" in the diagrams above are to comply with another part of [the ABI](http://www.uclibc.org/docs/psABI-x86_64.pdf): the stack must always be 16-byte aligned (i.e. <tt>rsp</tt> must have the lowest four bits set to zero). The compiler can generate Assembly code that leaves the stack pointer 8-byte aligned (e.g. if no static stack bytes were allocated, the 8-byte return address that the <tt>call</tt> instruction put on the stack means the stack pointer would not be 16-byte aligned). In fact, [OSX will terminate a process](http://blogs.embarcadero.com/eboling/2009/05/20/5607) that calls a dynamically-linked function when the stack isn't aligned correctly. When dealing with functions that dynamically allocate stack memory, we'll ensure that we always push a multiple of two arguments so we maintain 16-byte alignment - and if we're calling a function with an odd number of arguments we'll add a dummy zero parameter that the function being called will never read:

~~~~
@Ensure Stack Is Aligned@ +=
if(framePointer != null
&& values.size() > PARAMETERS.length
&& values.size() % 2 == 1)
  values.add(new Immediate(0));
~~~~

If a function's stack space is allocated statically then we don't need to do anything after a function call. The parameters that were passed will remain on the stack until overwritten by a subsequent function call, but when control flow returns from the function all the arguments will be discarded. However, if we're compiling a function that dynamically allocates stack memory we'll remove the parameters from the stack. We don't have to do this as subsequent dynamic allocations will just use the memory after the unneeded parameters, but we'll rather improve the stack's [spatial locality](http://en.wikipedia.org/wiki/Locality_of_reference) for the cost of an <tt>add</tt> instruction.

~~~~
@Remove Args From Stack@ +=
if(framePointer != null)
  append(new BinaryOp(
    Op.addq,
    new Immediate(storedArgs * SIZE), 
    rsp));
~~~~

The x86 <tt>mov</tt> instruction doesn't allow both operands to be indirect (i.e. values in memory), however the [push](http://ref.x86asm.net/coder64.html#x50) instruction allows registers and indirect values as its operand. This means if we're not using <tt>Push</tt> instructions to pass parameters on the stack (i.e. if we have dynamic stack memory allocations), so we'll have to load any parameter <tt>Value</tt>s that are in memory: 

~~~~
@Move Values From Memory@ +=
if(arg instanceof AtAddress && framePointer != null) {
  Variable tmp = variable(null); 
  append(move(arg, tmp));
  arg = tmp;
}
~~~~

### Stack Variable Allocation ###

There are diagrams of the two different stack layouts we can generate above. We'll go through how we do this now, but we'll start by calculating the amount of space we'll need to reserve in functions that only have static allocations so we can pass parameters to other functions on the stack. When we're compiling function calls we'll keep track of the most number of parameters passed on the stack that we've seen. We'll keep this in an <tt>Integer</tt> (not a primitive <tt>int</tt>) field so we can leave it as <tt>null</tt> if the function doesn't make any calls.

~~~~
@Parsing State Members@ +=
private Integer mostStackArgs;
@Function Call Bookkeeping@ +=
if(mostStackArgs == null)
  mostStackArgs = storedArgs;
else
  mostStackArgs = Math.max(mostStackArgs, storedArgs);
~~~~

A stack allocation expression is dynamic if we know the size at compile-time, which for this compiler means the argument to the <tt>$</tt> function is an <tt>Immediate</tt> (we said at the beginning this compiler isn't going to attempt to optimise the source code it's given - but [constant propagation](http://en.wikipedia.org/wiki/Constant_folding#Constant_propagation) may help turn dynamic allocations into static ones). Note that a function with dynamic stack allocations can also have static stack allocations, so the compiler will do the static allocation book-keeping for every function.

~~~~
@Parse A Stack Allocation@ +=
Variable ret = variable(null);
Value size = getAsValue(get(t, 1, 0));
if(size instanceof Immediate) {
  @Statically Allocate On Stack@
} else {
  @Dynamically Allocate On Stack@
}
return ret;
~~~~

To generate the Assembly code we just need the total number of bytes we need to allocate on the stack. We can use a counter of the numnber of bytes of memory we've allocated so far to generate the offset from the top of the stack of new allocations. This means we'll assign the first allocation the compiler comes across to the memory location nearest the top of the stack (so it has the lowest absolute memory address). The last static allocation we make will be in the memory location closest to the start of the stack (so the bytes immediately after the return address put on the stack by the <tt>call</tt> instruction).

The <tt>allocateOnStack</tt> method returns offsets from the top of the function's statically-allocated memory, but when we generate Assembly instructions that access it we have to determine which address to add these offsets to. For a function with no dynamic allocation we can just use the stack pointer, <tt>rsp</tt>, but when a function has  dynamic allocations the stack pointer can change value over the course of a function. Instead we'll use the frame pointer, but as this points to the return address at the other end of the statically-allocated memory (a higher address than the stack pointer) we'll have to add 8 bytes so we don't overwrite the return address.

~~~~
@Parsing State Members@ +=
private long stackBytes = 0;
long allocateOnStack(Immediate bytes) {
  long ret = stackBytes;
  stackBytes += bytes.value.get();
  return ret; 
}
~~~~

In the System V AMD ABI [the stack grows downwards](http://stackoverflow.com/a/1691818/42543), so we allocate <tt>n</tt> bytes of memory by subtracting <tt>n</tt> from the stack pointer. We can generate a pointer to the lowest address of the allocated memory by taking our "base pointer" (either the stack pointer or the frame pointer) and adding or subtracting (respectively) the offset. We always want the pointer we return to point at the lowest address in the block of memory that was allocated, so if we're counting down from the frame pointer we'll have to subtract an additional <tt>(n - SIZE)</tt> bytes to get there, as the offset would otherwise be pointing to the highest address in the allocated block. Also, if we're counting up from the stack pointer, we must remember to skip over the slots we've allocated for passing parameters to other functions on the stack.

~~~
@Statically Allocate On Stack@ +=
long offset = allocateOnStack((Immediate)size);
if(framePointer == null) {
  append(move(rsp, ret));
  append(new BinaryOp(
    Op.addq,
    new Immediate(new StaticStackVarLocation(this, offset)), 
    ret));
} else {
  append(move(framePointer, ret));
  append(new BinaryOp(
    Op.subq,
    new Immediate(offset + ((Immediate)size).value.get() + SIZE), 
    ret));
}
~~~~

As we're updating the <tt>mostStackArgs</tt> field as we come across function calls we don't know the final value of <tt>mostStackArgs</tt> until we've processed the whole function. We'll therefore use laziness to delay the calculation of the memory locations in functions that only statically allocate memory on the stack. 

~~~~
@Static Classes@ +=
static class StaticStackVarLocation implements com.google.common.base.Supplier<Long> {
  final ParsingState state; final long offset;
  StaticStackVarLocation(ParsingState state, long offset) {
    this.state = state; this.offset = offset;
  }
  public Long get() {
    if(state.mostStackArgs == null)
      return offset;
    else
      return offset + state.mostStackArgs * SIZE;
  }
}
~~~~

We can dynamically allocate memory easily by just subtracting the amount of memory we need from the stack pointer. We want the pointer we return to point to the lowest memory address of the block that we've just allocated, but since the stack grows downwards, and since the stack pointer points to an allocated byte of memory then we get these semantics for free. We can just copy the value of <tt>rsp</tt> after we've done the allocation (i.e. subtracted the size we want from the stack pointer) to the return <tt>Value</tt>.

~~~~
@Dynamically Allocate On Stack@ +=
append(new BinaryOp(Op.subq, size, Register.rsp));
@Align The Stack@
append(move(Register.rsp, ret));
~~~~

We've said above that if we call another function we need the stack pointer to be 16-byte aligned, and if a function has dynamic allocations we can ensure this in one of two ways; we can align the stack after every dynamic allocation or we can align the stack before each function call. In both cases "aligning the stack" means allocating between one and 15 bytes by using a <tt>and</tt> instruction to rounding the stack pointer down to a multiple of 16. The compiler currently generates Assembly code that  aligns the stack after every dynamic allocation as [on some CPUs](http://lemire.me/blog/archives/2012/05/31/data-alignment-for-speed-myth-or-reality) accessing aligned memory is faster than accessing unaligned memory.

Unfortunately, we can't just <tt>and</tt> the stack pointer with <tt>0xFFFFFFFFFFFFFF0</tt> (i.e. force the last 4 bits to zero) as only the <tt>mov</tt> x86-64 instruction can take 64-bit immediate operands. We could move <tt>0xFFFFFFFFFFFFFF0</tt> into a <tt>Variable</tt> (i.e. put it in a register) and then <tt>and</tt> it with the stack pointer, but the extra register could cause (slow) spills of other variables onto the stack. Instead we'll shift <tt>rsp</tt> right by four bits then left:

~~~~
@Align The Stack@ +=
alignStack();
@Parsing State Members@ +=
void alignStack() {
  append(new BinaryOp(Op.shrq, new Immediate(4), rsp));
  append(new BinaryOp(Op.shlq, new Immediate(4), rsp));
}
~~~~

We'll add a helper for when the register assignment algorithm needs to temporarily store some variables on the stack. While we could allocate a new 8-byte chunk of memory every time we wanted to "spill" a register to the stack, we know it's safe to re-use a stack allocation if we're spilling the same variable. If we wanted to be even more efficient, instead of using a single <tt>THESTACK</tt> dummy register to let the assignment algorithm tell us we need to spill a variable we could have one dummy register for each live variable (see section two for a definition of liveness), and add hints to the algorithm to make it re-use dummy registers whereever it can. We would then only need to allocate stack space for the dummy registers that were actually used - which should hopefully be fewer than one register per variable spilled.

~~~~
@Parsing State Members@ +=
final Map<Variable, Long> spilled = Maps.newTreeMap();
Supplier<Long> spillVariable(Variable v) {
  Long offset = spilled.get(v);
  if(offset == null)
    spilled.put(v, offset = allocateOnStack(new Immediate(SIZE)));
  if(framePointer == null)
    return new StaticStackVarLocation(this, offset);
  else
    return Suppliers.ofInstance(offset + 2 * SIZE);
}
~~~~

Now we have all we need to calculate the number of bytes of memory we should allocate when entering a function. 

One of the reasons we calculated <tt>mostStackArgs</tt> and set it to null if the function didn't call any others was so that we could take advantage of the [red zone](http://en.wikipedia.org/wiki/Red_zone_(computing)). This is a guarantee from [the ABI](http://www.uclibc.org/docs/psABI-x86_64.pdf) that no interrupt handler or other code will write to the (unallocated) 128 bytes below the stack pointer. If we know we're not going to trample on that memory later (via dynamic stack allocations or by calling other functions that will use it to store their return address & stack memory) then we don't need to explicitly allocate it when entering the function. If we were planning on allocating 128 bytes or less on the stack then we can skip the instructions that subtract and restore <tt>rsp</tt> altogether!

~~~~
@Parsing State Members@ +=
boolean useRedZone() {
  return mostStackArgs == null && framePointer == null;
}
~~~~

Another special case that will affect the number of bytes we have to allocate is the main function. When the main function is called by a new process on OSX the stack pointer in <tt>rsp</tt> is only 8-byte aligned, as this gdb session anecdotally shows:

<pre>
Starting program: /private/var/folders/HT/HTSjwIiwHaSNfbnpJnpWX++++TI/-Tmp-/2859 
Breakpoint 1, 0x0000000100010000 in main ()
(gdb) info register rsp
rsp            0x7fff5fbff858   0x7fff5fbff858
(gdb)
</pre>

However, when we return from the main function we should restore the original value of <tt>rsp</tt> (regardless of its alignment) otherwise we'll [segfault](http://en.wikipedia.org/wiki/Segmentation_fault) and the OSX will terminate the process. We'll therefore generate the code for the main function slightly differently. We'll push the old value of <tt>rsp</tt> onto the stack, and use a <tt>pop</tt> instruction to restore it to <tt>rsp</tt> when we leave. The <tt>Instruction</tt> <tt>alignStack</tt> returns will leave the stack 16-byte aligned, but we expect to be 8-byte aligned at this point (as a function starts 16-byte aligned but then has the 8 byte return address pushed on the stack). This means for the main function we'll allocate up to 15 extra bytes, but other functions we'll allocate up to 7 extra bytes to ensure that the stack is 16-bit aligned after the static memory allocation.

~~~~
@Parsing State Members@ +=
boolean isMainFn() {
  return myName.name.equals("_main");
}
@Are We Starting The Main Function@ +=
if(isMainFn()) {
  append(new Push(rsp));
  alignStack();
}
@Are We Leaving The Main Function@ +=
if(isMainFn())
  append(new Pop(rsp));
@Static Classes@ +=
private static final class Pop extends Instruction {
  final StorableValue value;
  Pop(StorableValue value) { this.value = value; }
  public String toString() { @Pop x86 Code@ }
}
~~~~

It's worth mentioning that the magic string "main" is imposed by the standard library our generated Assembly code should be linked against. The [standard library isn't necessary](https://blogs.oracle.com/ksplice/entry/hello_from_a_libc_free); the linker actually looks for a <tt>_start</tt> symbol as the entry point to a compiled programme. Libc's implementation of <tt>_start</tt> [initialises the standard library](http://stackoverflow.com/a/7977208/42543) before calling the <tt>_main</tt> function. 

We now have all the information we need to calculate the total number of static bytes. This logic is in a <tt>Supplier</tt> so we can delay evaluation until after the register assignment algorithm, as if that decides to "spill" registers onto the stack it'll allocate 8 bytes per register spilled, which increases the <tt>stackBytes</tt> field and so changes the number of bytes we need to allocate. We'll round the number of bytes we need up to the nearest multiple of 16 so the stack remains 16-byte aligned after the allocation. However, if this function doesn't call any others (<tt>mostStackArgs</tt> is null) then we don't care about alignment so don't need to round the stack pointer. This may save us from having to modify the stack pointer at all - if the function doesn't use any space on the stack but does make function calls we still need to subtract 8 from the stack pointer to maintain alignment. If a function doesn't use any space on the stack and doesn't make function calls we can skip at least two instructions (one for the initial allocation and one for each <tt>ret</tt>).

~~~~
@Static Classes@ +=
static final class StaticStackBytes implements com.google.common.base.Supplier<Long> {
  final ParsingState state;
  StaticStackBytes(ParsingState state) {
    this.state = state;
  }
  public Long get() {
    long ret = state.stackBytes;
    if(state.framePointer == null 
    && state.mostStackArgs != null)
      ret += state.mostStackArgs * SIZE;
    if (state.useRedZone())
      ret = Math.max(0, ret - 128);
    if(state.mostStackArgs == null) 
      return ret;
    long multipleOf16 = ret + 15 & 0xFFFFFFFFFFFFFFF0L;
    if(state.isMainFn())
      return multipleOf16;
    else
      return ret > multipleOf16 - 8 ? (multipleOf16 + 8) : (multipleOf16 - 8);
  }
}
~~~~

Now we can add Assembly instructions to subtract this number from <tt>rsp</tt> when entering a function and add it when leaving. However, if the function has dynamic stack allocation we must subtract the number of bytes we need from <tt>rsp</tt> after saving the initial value of <tt>rsp</tt> into the frame pointer variable. As we're ignoring the value of <tt>rsp</tt> when we replace it with the frame pointer on leaving the function we don't have to explicitly free the statically-allocated memory by adding to the stack pointer. We still need to do this for the main function even though we're going to overwrite <tt>rsp</tt> with the value we <tt>pop</tt> off the stack. <tt>pop</tt> just reads the top value of the stack, so if we've statically allocated stack variables we don't want to restore one of them to the stack pointer!

~~~~
@After Stack Allocation Setup@ +=
append(new BinaryOp(
  Op.subq,
  new Immediate(new StaticStackBytes(this)), 
  rsp));
@Before Returning From Function@ +=
if(framePointer == null)
  append(new BinaryOp(
    Op.addq,
    new Immediate(new StaticStackBytes(this)), 
    rsp));
~~~~

### Function Parameters ###

Working out the memory address of parameters that have been passed on the stack to the function we're compiling is much like working out the memory address of a variable allocated on the stack. We need to find the memory address of the top of the stack (the frame pointer if the function has dynamic stack allocation, the stack pointer plus the total amount of memory we've reserved for the function otherwise), and count 8 bytes into the previous stack frame each stack parameter. As above, if we need to know how many bytes this function's allocated on the stack we have to delay the calculation using the <tt>Supplier</tt> interface until after we've run the register assignment algorithm. Once we've found the start of the stack frame (regardless of method), we count <tt>(number + 1) * SIZE</tt> bytes upwards. <tt>number</tt> will be 0 for the seventh parameter of the function (the first one passed on the stack), 1 for the eighth, and so on. We need the <tt>+ 1</tt> as the top of the stack frame (where the frame pointer points) is the 8-byte return address, which we need to skip over.

~~~~
@Parsing State Members@ +=
StorableValue parameter(int number) {
  if (framePointer == null)
    return new AtAddress(
      rsp, 
      new Parameter(new StaticStackBytes(this), number));
  else
    return new AtAddress(
      framePointer, 
      (number + 1) * SIZE);
}
@Static Classes@ +=
static final class Parameter implements com.google.common.base.Supplier<Long> {
  final Supplier<Long> stackBytes; final int number;
  Parameter(Supplier<Long> stackBytes, int number) {
    this.stackBytes = stackBytes; this.number = number; 
  }
  public Long get() {
    return (number + 1) * SIZE + stackBytes.get();
  }
}
~~~~

When we first enter a function the parameter values will either be in the <tt>PARAMETERS</tt> registers or on the stack. We want to move these values into the <tt>Variable</tt>s used in the source code for the parameters. We'll insert <tt>mov</tt> instructions at the start of the function to save a copy of the parameter values passed in registers into their corresponding <tt>Variable</tt>s. However, for parameter values passed on the stack we have two options: we can similarly insert <tt>mov</tt> instructions at the start of the function to load the parameter values into registers from memory, or we can just replace all reads and writes to that parameter <tt>Variable</tt> in the function's code with reads and writes to the memory address where the parameter value was originally stored. Adding instructions at the start means each stack parameter adds another <tt>Variable</tt> to the function, which increases [register pressure](http://en.wikipedia.org/wiki/Register_pressure) as more <tt>Variable</tt>s are competing for space in the limited number of hardware registers. We'll make our compiler generate Assembly code for the latter - so we'll use the <tt>params</tt> field to map <tt>Variable</tt>s we come across in the source code to <tt>AtAddress</tt> <tt>Value</tt>s that point to where the parameter lives on the stack.

~~~~
@Parsing State Members@ +=
private Map<String, StorableValue> params = Maps.newTreeMap();
@Parse Function Parameters@ +=
for(int p = 0, count = numChildren(def, 1); p < count; ++p) {
  String v = text(def, 1, p).substring(1);
  if(p < PARAMETERS.length)
    append(move(PARAMETERS[p], variable(v)));
  else
    params.put(v, parameter(p - PARAMETERS.length));
}
~~~~

### Assignments ###

We can now cover the three remaining <tt>statement</tt> nodes. Assignments can change the value of a <tt>Variable</tt>, a value stored at a memory address a <tt>Variable</tt> points to or the value stored in memory pointed to by a <tt>Label</tt>. However, an assignment can't change the memory address a <tt>Label</tt> points to. 

~~~~
@Parse An Assign Statement@ +=
@Evaluate The Expression To Assign@
@Count Number Of Dereferences@
@Prepare Target@
@Prepare Source@
@Generate Mov Instructions@
~~~~

We'll start by evaluating the right hand side of the assignment statement - this is the value that we have to update the left hand side with.

~~~~
@Evaluate The Expression To Assign@ +=
Value expr = getAsValue(get(statement, 1));
~~~~

Assignment statements can contain arbitary levels of indirection; <tt>@@$a = 0x1;</tt> will store the value <tt>1</tt> at the memory address pointed to by the memory address in memory that variable <tt>a</tt> points to. This statement's AST will be:

<pre>
+--------+
|Variable|
|  "a"   |
+--------+
    ^
 +--+--+
 |Deref|
 +-----+
    ^      +---------+
 +--+--+   |Immediate|
 |Deref|   |   "1"   |
 +-----+   +---------+
    ^           ^
    +----+------+
      +--+---+
      |Assign|
      +------+
</pre>

We'll start by counting the number of <tt>Deref</tt> nodes on the left hand side of the assignment, as we'll have to translate these to a memory accesses.

~~~~
@Count Number Of Dereferences@ +=
int numDerefs = 0;
while(type(statement, 0) == Deref) {
  ++numDerefs; statement = get(statement, 0);
}
~~~~

In our example, <tt>statement</tt> would now be the <tt>Variable</tt> AST node (originally it pointed to the <tt>Assign</tt> node), and <tt>numDerefs</tt> would be two. We'll generate different code for <tt>Label</tt> and <tt>Variable</tt> assignments:

~~~~
@Prepare Target@ +=
StorableValue to = toStorable(getAsValue(get(statement, 0)));
if(to instanceof Label) {
  @Store Value In A Label@
} else {
  @Store Value In A Variable@
}
~~~~

If we're assigning to a <tt>Label</tt> it means we're storing a value into the memory address the <tt>Label</tt> points at. We'll wrap the <tt>Label</tt> in an <tt>AtAddress</tt> to represent this indirection. We need to declare all the global variables we export in the generated assembly code (we actually generate code to define them in the the <tt>.bss</tt> or <tt>.data</tt> segments - see section three of the article). The language we're compiling doesn't make a distinction between [declaring and defining](http://ee.hawaii.edu/~tep/EE160/Book/chap14/subsection2.1.1.4.html) a variable. We'll assume the first assignment to a global variable we see in the source code is the declaration. The <tt>program.globals</tt> map stores all the <tt>Label</tt>s we've seen so far, and we'll use this to decide if this is the first time we've seen a given <tt>Label</tt>. The initial value (zero in most cases) at the <tt>Label</tt> is stored in the Assembly code we generate.

~~~~
@Store Value In A Label@ +=
@Shortcut For Global Initialisation@
if(!program.globals.containsKey(to))
  program.globals.put((Label) to, new Immediate(0));
to = new AtAddress((Label) to, 0);
~~~~

Note that as we always wrap the <tt>Label</tt> in an <tt>AtAddress</tt> we treat an assignment to a <tt>Label</tt> with no levels of indirection (e.g. <tt>X = 0x1;</tt>) as an assignment with one level of indirection (e.g. <tt>@X = 0x1;</tt>). A spec for the language we're compiling should state that assigning to a label without indirection is undefined behaviour (or is just an error).

Storing an expression in a <tt>Variable</tt> just compiles to a <tt>mov</tt> instruction. However, if we have any direction we'll wrap <tt>to</tt> in an <tt>AtAddress</tt> to minimise the number of intermediate <tt>mov</tt> instructions we'll have to add to get the final memory destination as a <tt>StorableValue</tt> that we can copy the <tt>expr</tt> into.

~~~~
@Store Value In A Variable@ +=
if(numDerefs > 0)
  to = new AtAddress((Variable) to, 0);
~~~~

Again, if we're storing into a memory address we must ensure <tt>expr</tt> isn't also a memory address, as <tt>mov</tt> instructions can't have two indirect operands.

~~~~
@Prepare Source@ +=
if(to instanceof AtAddress)
  expr = toRegister(expr);
~~~~

x86 instructions don't support more than one level of indirection (e.g. you can have <tt>mov (%rax), %rax</tt> but not <tt>mov ((%rax)), %rax</tt>) we'll insert a <tt>mov</tt> instruction for each level of indirection after the first. These inserted instructions will update the location we're assigning to with the memory address at the location it pointed to, removing a layer of indirection. We can then add a <tt>mov</tt> instruction that copies <tt>expr</tt> to <tt>to</tt> as by this point both operands  have at most one level of indirection.

~~~~
@Generate Mov Instructions@ +=
for(; numDerefs > 1; --numDerefs) {
  Variable inter = variable(null);
  append(move(to, inter));
  to = new AtAddress(inter, 0);
}
append(move(expr, to));
~~~~

We mentioned above we give the variable an initial value zero when we declare it (implicitly the first time we see it). We'll make an exception to this for compile-time constants (which we'll define as the right-hand side of the assignment is an <tt>Immediate</tt>), and we'll embed that constant as initial value at the <tt>Label</tt> in the Assembly code generate. If we do use the right-hand side as the initial value we don't need to generate instructions to copy the immediate to the location of the <tt>Label</tt>, so we'll <tt>break</tt> out of the switch statement we're in.

~~~~
@Shortcut For Global Initialisation@ +=
if(numDerefs <= 1
&& expr instanceof Immediate 
&& !program.globals.containsKey(to)) {
    program.globals.put((Label) to, (Immediate)expr); break;
} 
~~~~

### Jumps ###

We'll use [jump](http://www.unixwiz.net/techtips/x86-jumps.html) instructions to do this. Jump instructions are like the conditional <tt>mov</tt> instructions we met above; as well as unconditional jumps we can also add a suffix to indicate the jump instruction should only be executed if certain flags in the [status register](http://en.wikipedia.org/wiki/Status_register) are set. As we're just jumping to different points in the same function the <tt>Jump</tt> instruction and the <tt>Label</tt> <tt>Definition</tt> we're jumping to will both be in the same <tt>text</tt> segment of the generated Assembly code, so we don't need to worry about [far jumps](http://stackoverflow.com/questions/14812160/near-and-far-jmps).

As well as the <tt>Definition</tt> of the <tt>Label</tt> the <tt>Jump</tt> jumps to, we'll also have members to store the <tt>Condition</tt> if the jump is conditional.

~~~~
@Static Classes@ +=
static class Jump extends Instruction {
  Definition to; final Condition cond;
  Jump(Definition to, Condition cond) { 
    this.to = to; this.cond = cond;
    @After Constructing A Jump@
  }
  public String toString() { @Jump x86 Code@ }
  @Jump Members@
}
~~~~

The language we're using restricts control flow a lot more strictly that x86 assembly does. Firstly, we'll ensure each <tt>Label</tt> definition is only referenced by at most one <tt>Jump</tt> <tt>Instruction</tt>. Jumps are reflected in the intrinsic doubly-linked list of <tt>Instruction</tt>s as extra links between pairs of <tt>Jump</tt> and <tt>Definition</tt> <tt>Instruction</tt>s. This restriction means each <tt>Definition</tt> only needs a single extra field to store a pointer to the relevant <tt>Jump</tt> <tt>Instruction</tt>; otherwise we'd need a collection of all the <tt>Jump</tt>s that reference the <tt>Definition</tt>.

~~~~
@Definition Members@ +=
Jump comeFrom = null;
@After Constructing A Jump@ +=
to.comeFrom = this;
~~~~

We know a <tt>Jump</tt> can only reach one <tt>Definiton</tt>, and one <tt>Definiton</tt> can only be reached by one <tt>Jump</tt>, so we'll override <tt>delete</tt> in both <tt>Instruction</tt> subclasses to make a delete on a <tt>Jump</tt> delete the corresponding <tt>Definition</tt> too, and vice versa. This means <tt>Instruction</tt> intrinsic linked list won't ever contain a <tt>Definition</tt> that nothing jumps to or a <tt>Jump</tt> whose <tt>Definition</tt> doesn't exist. Note that we'll have to set the <tt>to</tt> or <tt>comeFrom</tt> pointer to null before calling <tt>delete</tt> on the other <tt>Instruction</tt> of the pair to avoid an infinite recursion.

~~~~
@Definition Members@ +=
@Override Instruction delete() {
  if(comeFrom != null) {
    Instruction tmp = comeFrom;
    comeFrom = null;
    tmp.delete();
  }
  return super.delete();
}
@Jump Members@ +=
@Override Instruction delete() {
  if(to != null) {
    Instruction tmp = to;
    to = null;
    tmp.delete();
  }
  return super.delete();
}
~~~~

Like the <tt>Ret</tt> class, we know that if we're making an unconditional jump, then no <tt>Instruction</tt> after the <tt>Jump</tt> is reachable. We'll override <tt>append</tt> in same way as <tt>Ret</tt>, ignoring the new <tt>Instruction</tt> if we can't reach it.

~~~~
@Jump Members@ +=
@Override Instruction append(Instruction i, ParsingState state) {
  if(cond == null) {
    i.prev = null;
    return this;
  }
  else
    return super.append(i, state);
}
~~~~

Similarly, we'll update the <tt>next()</tt> and <tt>prev()</tt> definitions to take into account that, for a <tt>Jump</tt>, the next <tt>Instruction</tt> could be one of two choices (or only one if the <tt>Jump</tt> is unconditional), and that a <tt>Defintion</tt> always has two predecessors; the <tt>Jump</tt> that refers to it and the directly preceding <tt>Instruction</tt>. These methods return <tt>Set</tt>s so we cover the case where the <tt>Definition</tt> comes directly after the <tt>Jump</tt> referring to it.

~~~~
@Jump Members@ +=
Set<Instruction> next() {
  if(cond != null && next != null)
    return ImmutableSet.of(next, to);
  else
    return ImmutableSet.of(to);
}
@Definition Members@ +=
Set<Instruction> prev() {
  if(prev != null)
    return ImmutableSet.of(prev, comeFrom);
  else
    return ImmutableSet.of(comeFrom);
}
~~~~

### If Statements ###

If-statements (with an optional else-block) are the first <tt>Statement</tt> we'll cover that allows control flow to jump to different locations in a function. We'll compile if and if-with-else blocks slightly differently. The two lists of Assembly instructions below show that if we have an else block we need to append an unconditional jump to the end of the code in the if-block so that we skip over the instructions in the else-block. If the <tt>cmp</tt> fails when we have an else-block we'll jump to the start of that (rather than the <tt>end</tt> <tt>Label</tt> added at the end of the statement), and control flow will run straight through to the <tt>end</tt> <tt>Label</tt>.

<pre>
if >(x, 0) { xxx }
else { yyy }          if >(x, 0) { ... }

  +--------+             +--------+
  |cmp x, 0|             |cmp x, 0|
  +--------+             +--------+
  |jbe else|             |jbe end |
  +--------+             +--------+
  |  xxx   |             |  ...   |
  +--------+             +--------+
  |jmp end |             |end:    |
  +--------+             +--------+
  |else:   |
  +--------+
  |  yyy   |
  +--------+
  |end:    |
  +--------+
</pre>

We'll put this logic in a separate <tt>parseIf</tt> method as we'll re-use it when parsing while-statements.

~~~~
@Parsing State Members@ +=
private Label parseIf(Tree test, Tree ifTrue, Tree ifFalse) {
  @Do If Comparison Check@
  Label end = new Label();
  if(ifFalse == null) {
    @Parse An If With No Else@
  } else {
    @Parse An If With An Else@
  }
  return end;
}
~~~~

We don't want to generate dead code if we can avoid it, so we'll use the method below to see if we can determine whether the if statement's test expression is a compile-time constant (so the same branch of code will always be executed at runtime).

~~~~
@Other Helpers@ +=
static boolean isConstant(Tree t) {
  return type(t) == Number;
}
static boolean isNeverZero(Tree t) {
  return isConstant(t) && parseHex(t) != 0;
}
~~~~

We'll just generate Assembly code for the if-block or else-block as appropriate:

~~~~
@Parse An If Statement@ +=
Tree 
  test = get(statement, 0), 
  ifTrue = get(statement, 1), 
  ifFalse = numChildren(statement) == 3 ? get(statement, 2) : null;
if(isConstant(get(test,0))) {
  @Skip If Statement@
} else {
  Label endIf = parseIf(test, ifTrue, ifFalse); 
  append(new Definition(endIf, false));
}
~~~~

However, if the test is a constant we can do a compile-time optimisation by just inlining whichever branch will (always) be chosen (and we'll not generate code for the test).

~~~~
@Skip If Statement@ +=
if(isNeverZero(get(test, 0)))
  parseStatements(children(ifTrue));
else if(ifFalse != null)
  parseStatements(children(ifFalse));
~~~~

In both types of if-statement we only execute the jump following the <tt>cmp</tt> if the comparison failed. This means we'll need to use the inverse of the <tt>Condition</tt> we're testing for as the <tt>Jump</tt>'s condition.

~~~~
@Condition Members@ +=
Condition inverse() {
  switch(this) {
    case b:  return ae;
    case a:  return be;
    case e:  return ne;
    case ne: return e;
    case be: return a;
    case ae: return b;
  }
  throw new IllegalStateException();
}
~~~~

If the "test" expression is already a <tt>Conditional</tt> intrinsic function call we will have generated conditional <tt>mov</tt> instructions when parsing it. We'll re-use the comparison by replacing the two <tt>mov</tt> Assembly instructions that copy an <tt>Immediate</tt> zero or one into the <tt>Variable</tt> the expression evaluates to with a <tt>Jump</tt>. If the "test" expression evaluates to some other <tt>Value</tt> we'll compare it to zero, so we'll execute the if-block if the <tt>Value</tt> is zero (just like C's semantics). We must ensure that the return value of the expression is a <tt>StorableValue</tt> as the x86 <tt>cmp</tt> instruction doesn't allow two <tt>Immediate</tt> operands.

~~~~
@Do If Comparison Check@ +=
StorableValue trueOrFalse = toStorable(getAsValue(get(test, 0)));
Condition jumpCond;
if(current instanceof Move && ((Move) current).cond != null) {
  jumpCond = ((Move) current).cond;
  current = current.delete().delete();
} else {
  jumpCond = Condition.e;
  append(new BinaryOp(Op.cmpq, new Immediate(0), trueOrFalse));
}
~~~~

If we don't have an else-block we'll jump to the <tt>end</tt> <tt>Label</tt> we created above. We can re-use the <tt>parseStatements</tt> function (i.e. we'll call it recursively) to generate <tt>Instruction</tt>s for the contents of the if-block.

~~~~
@Parse An If With No Else@ +=
append(new Jump(end, jumpCond.inverse(), FORWARDS));
parseStatements(children(ifTrue));
~~~~

If we have an else-block there's another edge case: if the last instruction of the if-block is a <tt>ret</tt> then we don't need to add the unconditional jump at the end of the if-block as we'll never reach it. Instead we'll generate code like:

<pre>
if >(x, 0) {
  xxx; return 0x1;
} else {
  yyy;
}

  +----------+
  |cmp x, 0  |
  +----------+
  |jbe else  |
  +----------+
  |  xxx     |
  +----------+
  |mov 1, rax|
  +----------+
  |ret       |
  +----------+
  |else:     |
  +----------+
  |  yyy     |
  +----------+
</pre>

If we need to generate Assembly code following this pattern we don't need to do anything differently at this stage; the dead code elimination step in section two of the article will remove the unreachable unconditional jump and then the definition of the <tt>Label</tt> that no instruction jumps to.

~~~~
@Parse An If With An Else@ +=
Label ifFalseLabel = new Label();
append(new Jump(ifFalseLabel, jumpCond.inverse(), FORWARDS));
parseStatements(children(ifTrue));
append(new Jump(end, null, FORWARDS));
append(new Definition(ifFalseLabel, false));
parseStatements(children(ifFalse));
~~~~

### While Statements ###

The language we're compiling only needs while and assignment <tt>Statement</tt>s to be [turing-complete](http://programmers.stackexchange.com/a/132420). Like if-statements, we'll parse <tt>While</tt> statements in one of two ways. If the test expression is a compile-time constant (an <tt>Immediate</tt> object, or a <tt>Number</tt> node in the AST) the loop will either never be executed (so we shouldn't generate any Assembly instructions) or will always be executed (so we don't need to generate instructions that test the value to check it's not zero each time we go round the loop). If the test isn't a compile-time constant we'll have to evaluate it each time we go round the loop.

~~~~
@Parse A While Statement@ +=
Label loopTop = new Label();
if(isConstant(get(statement, 0, 0))) {
  @Parse An Unconditional Loop@
} else {
  @Parse A Conditional Loop@
} 
~~~~

If we've decided an unconditional loop is executed (the test expression evaluates to a non-zero <tt>Immediate</tt>) then we'll put the <tt>loopTop</tt> <tt>Label</tt> before the Assembly instructions we'll generate for the while-statement's body and add an unconditional jump to the <tt>loopTop</tt> <tt>Label</tt> after them:

~~~~
@Parse An Unconditional Loop@ +=
if(isNeverZero(get(statement, 0, 0))) {
  append(new Definition(loopTop, false));
  parseStatements(children(get(statement, 1)));
  append(new Jump(loopTop, null, BACKWARDS));
}
~~~~

We'll compile a conditional loop using the same <tt>parseIf</tt> statement that we used to compile if-statements. <tt>parseIf</tt> returns the <tt>Label</tt> that should go at the end of the if-statement, but it doesn't add the <tt>Label</tt>'s <tt>Definition</tt> instruction to the generated Assembly code. When we're generating code for if-statements we just put this <tt>Label</tt> right after the code we generated for the else-block (or the if-block if there isn't an else-block). However, for while-statements we'll put this <tt>Label</tt> outside the loop, so the generated code will look like:

<pre>
while >(x, 0) {
  ...
}

  +--------+
  |top:    |
  +--------+
  |cmp x, 0|
  +--------+
  |jbe end |
  +--------+
  |  ...   |
  +--------+
  |jmp top |
  +--------+
  |end:    |
  +--------+
</pre>

If the test expression is false (the <tt>cmp</tt> instruction in the diagram above) then control flow will jump outside the loop block; if not the instructions in the loop block will be executed then control flow will jump back to the <tt>Label</tt> just before the test (the <tt>top</tt> label in the diagram above), and the test will be executed again.

~~~~
@Parse A Conditional Loop@ +=
append(new Definition(loopTop, false));
Label endWhile = parseIf(get(statement, 0), get(statement, 1), null);
append(new Jump(loopTop, null, BACKWARDS)); 
append(new Definition(endWhile, false));
~~~~

## (II) Register Allocation via Linear Programming ##

We now need to change the <tt>Variable</tt>s our <tt>Instruction</tt>s generate into <tt>Register</tt>s. Our choice of registers will affect the performance of the code we generate, as there's only a finite number of registers we can use, and all the <tt>Variable</tt>s we don't have room to store in these <tt>Register</tt>s will have to be stored on the stack, and we'll have to use expensive memory accesses whenever we need to read from or write to them. We can phrase the problem of register assignment as a [linear programming](???) problem as [this paper](http://grothoff.org/christian/lcpc2006.pdf) describes, which will give us an assignment of <tt>Variable</tt>s to <tt>Register</tt>s with the fewest spills to memory. We'll run this register allocation function-by-function, as the locations of <tt>Variable</tt>s passed between functions is fixed by the ABI, so we are only free to decide their location within a single function.

We can use 15 of the x86-64 architecture's registers as general purpose registers to store <tt>Variable</tt>s in:

~~~~
@Other Helpers@ +=
static final Register[] ASSIGNABLE = { 
  rax, rbx, rcx, rdx, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15, THESTACK 
};
~~~~

We'll use the Java [SWIG bindings](http://www.xypron.de/projects/linopt/apidocs/index.html) for the [GLPK](???) to actually solve the linear programming problem. A linear programming problem basically asks for a value for each of a set of variables to minimise (or maximise) a particular function (the "objective function"), subject to a series of constraints on the values of these variables. For our problem of variable assignment, most of the variables are going to be of the form "is variable _x_ in register _y_ at this point?". We need a clear yes-or-no answer, so we need to constrain the values that the linear programming variable can to *integers* between 0 and 1 (inclusive), which we'll interpret as a yes or no accordingly. The requirement that the value is an integer means our problem is actually an [integer programming](http://en.wikipedia.org/wiki/Integer_programming) problem.

~~~~
@Parsing State Members@ +=
glp_prob allocation = glp_create_prob();
~~~~

### Variable Reads and Writes ###

We'll now start to draw together the information we need to calculate when variables are "live"; when a previous <tt>Instruction</tt> has written to them and a future <tt>Instruction</tt> reads from them we have to ensure we don't erase the variable's value in the current <tt>Instruction</tt>. If a variable is written to twice without an intervening read then we'll consider the variable "dead" between the two writes. Now, some of the <tt>Instruction</tt>s we introduced above are conditional, and so may not always write to a variable. If a variable is written to twice (without an intervening read), and the second write is conditional, then the variable is "live" between the two writes as in some cases the variable will still have the value written to it by the first write after the second. We'll use an enum to distinguish whether an <tt>Instruction</tt> <tt>WILL_ERASE</tt> the value of a variable, or is condition and only <tt>CAN_ERASE</tt> the variable's value:

~~~~
@Other Helpers@ +=
enum WriteType { WILL_ERASE, CAN_ERASE };
~~~~

We'll expose which <tt>Variable</tt>s and <tt>Register</tt>s an <tt>Instruction</tt> affects by adding <tt>readsFrom</tt> and <tt>writesTo</tt> members. We'll also add a <tt>uses</tt> method as a shorthand for 'all the variables we'll ever touch'.

~~~~
@Other Helpers@ +=
static Set<Value> none = Collections.emptySet();
@Instruction Members@ += 
Set<Value> readsFrom() { return none; }
Set<Value> writesTo(WriteType t) { return none; }
Set<Value> uses() { return Sets.union(readsFrom(), writesTo(CAN_ERASE)); }
~~~~

Now, if an <tt>Instruction</tt>'s operand is indirect (which we represent as an instance of <tt>AtAddress</tt>) then we need to pull the <tt>Variable</tt> or <tt>Register</tt> that stores the memory address out of the <tt>AtAddress</tt> instance. We'll do this by getting a static helper address to downcast and extract the <tt>to</tt> field of <tt>AtAddress</tt> if appropriate, but a more object-oriented solution would add a <tt>Value unwrap()</tt> method to the <tt>Value</tt> interface, and identity implementations for all subclasses apart from <tt>AtAddress</tt>.

~~~~
@Other Helpers@ +=
static Value unwrap(Value v) {
  return v instanceof AtAddress ? ((AtAddress)v).to : v;
}
~~~~

Most implementations of <tt>readsFrom</tt> are straightforward:

~~~~
@BinaryOp Members@ +=
Set<Value> readsFrom() { return ImmutableSet.of(unwrap(arg1), unwrap(arg2)); }
@Push Members@ +=
Set<Value> readsFrom() { return ImmutableSet.of(unwrap(value)); }
~~~~

Writes are more interesting; if the target operand is indirect (an <tt>AtAddress</tt>) then it isn't written to - the memory location which the operand contains is. The <tt>Move</tt> <tt>Instruction</tt>'s <tt>writesTo</tt> implementation will return an empty set if the <tt>Instruction</tt> is conditional (i.e. the <tt>cond</tt> has one of the <tt>Condition</tt> enum values) and the caller asks for all the <tt>Value</tt>s the <tt>Instruction</tt> will definitely erase.

~~~~
@BinaryOp Members@ +=
Set<Value> writesTo(WriteType t) { 
  return arg2 instanceof AtAddress ? none : ImmutableSet.<Value>of(arg2); 
}
@Move Members@ +=
Set<Value> writesTo(WriteType t) {
  return to instanceof AtAddress || (t == WILL_ERASE && cond != null) 
    ? none : ImmutableSet.<Value>of(to); 
}
~~~~

Finally, a <tt>Move</tt> <tt>Instruction</tt> usually only reads from the source operand, but if the destination operand is indirect the <tt>Instruction</tt> will read the memory address to write the source value to from <tt>Variable</tt> or <tt>Register</tt> in the target operand.

~~~~
@Move Members@ +=
Set<Value> readsFrom() {
  return to instanceof AtAddress 
    ? ImmutableSet.of(unwrap(from), unwrap(to)) 
    : ImmutableSet.of(unwrap(from)); 
}
~~~~

### Liveness ###

A <tt>Variable</tt> is "live" at an <tt>Instruction</tt> if we care about what value the <tt>Variable</tt> contains immediately before the <tt>Instruction</tt> is executed. <tt>Variable</tt>s become live as the function writes values to them, and <tt>Variable</tt>s are no longer live when every control flow path doesn't read from them. We'll use liveness to limit the size of the linear programming problem we'll construct. Liveness analysis will also help reduce memory pressure as if we just stored every <tt>Variable</tt> regardless of whether we needed it, we'd have more competition for space in the registers, so more <tt>Variable</tt> accesses would require reading from memory.

We'll use a <tt>OpenBitSet</tt>s ([lucene's](???) bitset implementation) at each <tt>Instruction</tt> to store the ids of <tt>Variable</tt>s that are live at that <tt>Instruction</tt>. Because of the way we choose the <tt>id</tt>s of <tt>Variable</tt>s we know that they'll be integers between zero (inclusive) and the number of <tt>Variable</tt>s in the function (exclusive). This means an <tt>OpenBitSet</tt> is a very dense way of storing liveness as it only needs one bit per <tt>Variable</tt> in the function (you can think of it as a <tt>Variable</tt> being in a member of the set of live <tt>Variable</tt>s at that <tt>Instruction</tt>), even if it isn't sparse. However, <tt>OpenBitSet</tt> isn't that easy to work with as it goes to great lengths to avoid [boxing and unboxing](???) integers, so we'll wrap it in an adapter to Java's usual <tt>Set</tt> interface.

~~~~
@Static Classes@ +=
static class VariableSet extends AbstractSet<Variable> {
  private final OpenBitSet bits = new OpenBitSet();
  public boolean add(Variable e) {
    bits.ensureCapacity(e.id);
    return !bits.getAndSet(e.id);
  }
  public int size() {
    return (int) bits.cardinality();
  }
  public Iterator<Variable> iterator() {
    return new Iterator<Variable>() {
      int next = bits.nextSetBit(0);
      public void remove() {
        bits.fastClear(next);
      }
      public Variable next() {
        int ret = next;
        next = bits.nextSetBit(next + 1);
        return new Variable(ret);
      }
      public boolean hasNext() {
        return next >= 0;
      }
    };
  }
}
@Instruction Members@ +=
final VariableSet isLive = new VariableSet();
~~~~

When an <tt>Instruction</tt> is added to or removed from a chain of <tt>Instruction</tt>s via <tt>append</tt> or <tt>delete</tt> we'll work out the 'delta', or changes between what <tt>Variable</tt>s were live at the <tt>Instruction</tt> and what <tt>Variable</tt>s are live after it. We'll map this 'delta' to a series of calls to <tt>variableIsLive</tt> and <tt>variableIsDead</tt> so later parts of the code can act on this notification.

We're defining "liveness" such that if the current <tt>Instruction</tt> reads a <tt>Variable</tt> then that <tt>Variable</tt> is "live" at that <tt>Instruction</tt>; if the current <tt>Instruction</tt> writes to (and doesn't read) a <tt>Variable</tt> then the <tt>Variable</tt> is no longer "live" (i.e. has been "killed"), and otherwise the <tt>Variable</tt> is live if it is live in any of the <tt>Instruction</tt>s following the one we're considering.

~~~~
@Instruction Members@ +=
Set<Value> killed() {
  return Sets.difference(
    i.writesTo(WILL_ERASE), 
    i.readsFrom());
}
~~~~

We decided earlier that when we <tt>append</tt>ed an <tt>Instruction</tt> we were going to calculate what liveness information it changed, and then use the [visitor pattern](???) to propagate this information as far back as we can, instead of pushing liveness information from the current <tt>Instruction</tt> to the <tt>append</tt>ed one. This definition of <tt>killed</tt> shows why: consider two writes to a <tt>Variable</tt> followed by a read, with no intervening <tt>Jump</tt> <tt>Instruction</tt>s or other changes of control flow. We don't want to mark this <tt>Variable</tt> as live between the two writes as we'll just overwrite the value it contains without looking at it, and marking it as live means we have to waste space (maybe even register space) storing the value. Pulling liveness information backwards means we'll only mark it live between the read and the second write that "kills" it. Pushing liveness forwards would mean it'd be marked as live from the first write to the read. However, we said at the beginning we weren't going to do any optimisation, so we won't remove any useless writes we come across.

~~~~
@Instruction Members@ +=
boolean pullBackwards(ParsingState state) {
  boolean anyChanged = false;
  @Calculate Variables Propagated@
  if(anyChanged)
    updateLP(state);
  return anyChanged;
}
~~~~

<tt>pullBackwards</tt> returns <tt>true</tt> if any <tt>Variable</tt>s have changed liveness (either have become alive or been made dead) because of this call. In this case we'll have to continue propagating changes to the <tt>Instruction</tt>'s predecessors. We don't want to keep iterating when all we're doing is marking <tt>Variable</tt>s live that are already marked as live, so we'll also stop at a particular <tt>Instruction</tt> if all of the <tt>nowLive</tt> <tt>Variable</tt>s we're propagated are all live.

~~~~
@Instruction Members@ +=
void propagate(ParsingState state) {
  if(pullBackwards(state))
    for(Instruction i : prev())
      i.propagate(state);
}
~~~~

Liveness will be only be computed once, in the <tt>append</tt> method. This minimalises the work we have to do each <tt>append</tt>; if the newly-appended <tt>Instruction</tt> makes a <tt>Variable</tt> live, we'll iterate backwards through every code path from the new <tt>Instruction</tt>, marking that <tt>Variable</tt> as live, until we get to an <tt>Instruction</tt> that kills it (i.e. it writes to that <tt>Variable</tt> but doesn't read it).

There's one edge case here: if we don't propagate any <tt>Variable</tt>s from the <tt>append</tt>ed <tt>Instruction</tt> to this <tt>Instruction</tt> then <tt>pullBackwards</tt> will return true and the recusive visit will terminate - even if we should propagate <tt>Variable</tt>s the current <tt>Instruction</tt> makes live to the previous <tt>Instruction</tt>! We'll therefore ignore the return value of <tt>pullBackwards</tt> on the <tt>Instruction</tt> that was appended to, and call <tt>propagate</tt> on its predecessors regardless.

~~~~
@After Append@ +=
pullBackwards(state);
for(Instruction i : prev())
  i.propagate(state);
~~~~

Calculating the <tt>Variable</tt>s propagated is pretty straightforward; it's the union of all the <tt>Set</tt>s of live <tt>Variable</tt>s from the <tt>Instruction</tt>'s sucessors (which could be an empty set, if the <tt>Instruction</tt>'s <tt>next</tt> pointer isn't connected to anything).

~~~~
@Calculate Variables Propagated@ +=
Set<Value> propagated = readsFrom();
for(Instruction i : next())
  propagated = Sets.union(propagated, i.isLive);
Set<Variable> live = onlyVariables(
  Sets.difference(propagated, killed()));
anyChanged |= isLive.addAll(live);
anyChanged |= isLive.retainAll(live);
~~~~

Handling the deletion of an <tt>Instruction</tt> is much easier; we can just force the <tt>Instruction</tt> that was our predecessor to <tt>propagate</tt>, or synchronize its liveness information with its new sucessor <tt>Instruction</tt>. We can say <tt>Instruction</tt> in the singular here as when we remove a <tt>Jump</tt> (the only <tt>Instruction</tt> with more than one successor) or a <tt>Definition</tt> (the only <tt>Instruction</tt> with more than one predecessor) we automatically remove the other <tt>Instruction</tt> of the pair, so we never have to deal with a deletion that leaves two control flow paths dangling.

~~~~
@After Delete@ +=
oldPrev.propagate(state);
~~~~

We'll finish this section with a note about initialization. After an <tt>Instruction</tt> has been instantiated, but before it has been <tt>append</tt>ed to another <tt>Instruction</tt> (or been <tt>append</tt> to) the contents of the <tt>isLive</tt> set won't be correct; it'll still be empty but any <tt>Variable</tt>s the <tt>Instruction</tt> writes to are actually live. Also, the <tt>Ret</tt> <tt>Instruction</tt>s will never do this analysis as it overrides the logic in <tt>append</tt> in its subclass and doesn't call the <tt>super</tt> implememtation. This means that a <tt>Ret</tt>'s <tt>isLive</tt> field will always be empty - but this is correct, as no <tt>Instruction</tt>s are live just before control flow returns.

### Registers In Use ###

We know some instructions pre-specify <tt>Register</tt>s that they'll write to (e.g. the return value is put in <tt>rax</tt>, and the values in the <tt>CALLEE_SAVES</tt> <tt>Register</tt>s), and we don't want to overwrite these values by assigning another <tt>Variable</tt>s to those <tt>Register</tt>s. We'll build up a set of what <tt>Register</tt>s we shouldn't use at each <tt>Instruction</tt> in a member, following the pattern of the liveness analysis above.

~~~~
@Instruction Members@ +=
final Set<Register> inUse = EnumSet.noneOf(Register.class);
~~~~

However, there's one key different between the liveness ananlysis and the registers-in-use analysis: the state in <tt>Ret</tt> <tt>Instruction</tt>s. As we only do the analysis for an <tt>Instruction</tt> when <tt>append</tt> is called, we'll never do analysis on <tt>Instruction</tt>s that are never appended to - which, do to the way we translate our language into <tt>Instruction</tt>s in section one, are only <tt>Ret</tt>s. This doesn't matter in our liveness analysis as no <tt>Variable</tt>s are live at a <tt>Ret</tt>, but many <tt>Register</tt>s are in use at a <tt>Ret</tt> (i.e. the <tt>CALLEE_SAVES</tt> ones and some of the <tt>RETURNS</tt> ones, see later). 

One example of an <tt>Instruction</tt> that needs other <tt>Register</tt>s is <tt>Ret</tt>. Earlier we generated <tt>Instruction</tt>s to save and restore the values in the <tt>CALLEE_SAVES</tt> <tt>Register</tt>s after the <tt>Head</tt> <tt>Instruction</tt> and before each <tt>Ret</tt>. However, we don't want the register allocation to overwrite these values either before we've captured them into a <tt>Variable</tt> or after we've restored them to a <tt>Register</tt>, so we'll mark those <tt>Register</tt>s as "in use" when they have the preserved values. We get the preservation at the start of the function for free, as the <tt>Move</tt> <tt>Instruction</tt> that captures them marks them as "in use" as they're returned from <tt>registersInUse</tt>, and the <tt>PropagateInUse</tt> visitor will mark them in use in all <tt>Instruction</tt>s up to and including the <tt>Head</tt>, as we can guarantee no intervening <tt>Instruction</tt> reads from them. At the other end of the function, we can make <tt>Ret</tt> <tt>Instruction</tt> initialize <tt>inUse</tt> to the <tt>CALLEE_SAVES</tt> <tt>Register</tt>s, and that <tt>inUse</tt> flag will be propagated for each <tt>Register</tt> back as far as the <tt>Move</tt> <tt>Instruction</tt> that restores the corresponding preserved value. We'll also mark the <tt>RETURNS</tt> <tt>Register</tt>s as in use, so similarly <tt>Instruction</tt> will overwrite the values put there by a <tt>Return</tt> statement before control flow returns from the function.

~~~~
@Ret Constructor@ +=
inUse.addAll(Arrays.asList(CALLEE_SAVES));
for(int r = 0; r < varsReturned; ++r)
  inUse.add(RETURNS[r]);
updateLP(state);
~~~~

Function calls are another type of <tt>Instruction</tt> that has special behaviour. We'll try to make this behaviour as clear as possible, by breaking it down into several parts. The call return value is easy to fit into the existing <tt>Instruction</tt> - we'll just override the <tt>writeTo</tt> method to tell it that the <tt>Call</tt> writes its return value to <tt>rax</tt>:

~~~~
@Call Members@ +=
Set<Value> writesTo(WriteType t) {
  return ImmutableSet.<Value>of(rax);
}
~~~~

The second set of <tt>Register</tt>s we can't store values in are those that store the arguments we're passing the function to, and the name of the function we're calling (if it's directly specified as being in a <tt>Register</tt>). We can fit these <tt>Register</tt>s into the existing <tt>readsFrom</tt> method easily too. when we construct <tt>Call</tt> instances we give them the number of arguments we're passing to them in registers, which the <tt>Call</tt> object stores in its <tt>registerArgs</tt> int field.

~~~~
@Call Members@ +=
Set<Value> readsFrom() {
  Set<Value> ret = Sets.newHashSet(name);
  for(int r = 0; r < registerArgs; ++r)
    ret.add(PARAMETERS[r]);
  return ret;
}
~~~~

The calling convention of the [x86-64 ABI](???) allows us to put values in the <tt>CALLEE_SAVES</tt> <tt>Register</tt>s before a call, and the function we're calling will have to preserve those values. The function we call will also have its own stack frame, so any values we store on the stack (which we represent as assigning them to the dummy <tt>THESTACK</tt> <tt>Register</tt>) will also be preserved. We can't fit these semantics into either the <tt>readsFrom</tt> or <tt>writesTo</tt> methods. We can't add them as special cases to the constructor (like we did with <tt>Ret</tt>) as these restrictions only apply to the <tt>Call</tt> <tt>Instruction</tt>; adding the <tt>CALLEE_SAVES</tt> <tt>Register</tt>s to the <tt>inUse</tt> set would mean they'll get propagated. Instead we'll force access to the <tt>inUse</tt> set to go through an accessor method, and override this method in the <tt>Call</tt> subclass to get the behavour we want:

~~~~
@Instruction Members@ +=
Set<Register> inUse() {
  return inUse;
}
@Call Members@ +=
Set<Register> inUse() {
  Set<Register> ret = EnumSet.allOf(Register.class);
  ret.removeAll(Arrays.asList(CALLEE_SAVES));
  ret.remove(THESTACK);
  return Sets.union(
    inUse,
    ret);
}
~~~~

We can now slot the logic to update the <tt>inUse</tt> set into <tt>pullBackwards</tt>, so we do this analysis at the same time as we do the liveness analysis (saving an extra set of <tt>propagate</tt> calls). Also, since the <tt>killed</tt> set we calculated at the start of <tt>pullBackwards</tt> contains all <tt>Value</tt>s killed, not just <tt>Variable</tt>s, we can re-use that. We'll update the same <tt>anyChanged</tt> flag that the liveness analysis used, so we'll continue propagating backwards if either the liveness information or the registers-in-use information (or both) changed.

~~~~
@Calculate Variables Propagated@ +=
Set<Value> propagatedUse = readsFrom();
for(Instruction i : next())
  propagatedUse = Sets.union(propagatedUse, i.inUse);
Set<Register> used = Sets.filter(
  Sets.difference(propagatedUse, killed),
  Predicates.instanceOf(Register.class));
anyChanged |= inUse.addAll(used);
anyChanged |= inUse.retainAll(used);
~~~~

### What Needs Assigning ###

We've calculated the live <tt>Variable</tt>s at each <tt>Instruction</tt>, but these are just a subset of the <tt>Variable</tt>s we need to assign to <tt>Register</tt>s. <tt>Variable</tt>s that are dead immediately before an <tt>Instruction</tt> and are written to by the <tt>Instruction</tt> still need to be assigned so the CPU knows write the results out to. We'll use a similar method to <tt>inUse()</tt> above; we'll add an accessor to <tt>isLive</tt> that adds in these additional <tt>Variable</tt>s:

~~~~
@Other Helpers@ +=
static Set<Variable> onlyVariables(Set<Value> vs) {
  return Sets.filter(
    uses(),
    Predicates.instanceOf(Variable.class));
}
@Instruction Members@ +=
Set<Variable> isLive() {
  return Sets.union(
    isLive,
    onlyVariables(uses()));
}
~~~~

We've now calculated the live <tt>Variable</tt>s and <tt>Register</tt>s in use at each <tt>Instruction</tt>, but not every combination of the two sets is valid. The register assignment problem is basically "should we assign <tt>Variable</tt> v to <tt>Register</tt> r at <tt>Instruction</tt> i". We'll pose this problem as an Integer Programming problem to find the most efficient solution below, but at this point we can quickly eliminate some impossible (v, r, i) combinations. This is why we've been building up detailed control-flow information; the more solutions we rule out at this stage the smaller our Integer Programming problem will be, so the faster we can solve it. We'll also use this opportunity to eliminate some  <tt>Variable</tt>-<tt>Register</tt> combinations that are forbidden by the x86-64 ABI. This method is only for <tt>Variable</tt>s in <tt>isLive()</tt> and <tt>Register</tt>s that are not in <tt>inUse()</tt>; other combinations are definitely not valid.

~~~~
@Instruction Members@ +=
boolean isAssignable(Variable v, Register r) {
  @Is This Combination Impossible@
  return true;
}
~~~~

If a <tt>Variable</tt> is used by the <tt>Instruction</tt> we're considering (i.e. the <tt>isUsed</tt> flag we calculated at the start of this function is true), then we can't assign this <tt>Variable</tt> to the stack. If the <tt>Instruction</tt> is a <tt>BinaryOp</tt> and the other operand is indirect, then if we assigned the other operand to the stack then we'd have two indirect operands, which is forbidden by the x86 instruction set. However, if both operands are direct then we could assign one of the to the stack - but this would complicate our linear programming problem as we'd have to penalise assignments (as we'd prefer solutions where the <tt>Variable</tt>s that are used are assigned to <tt>Register</tt>s). We'll therefore take the simple approach, and just forbid these technically valid edge cases:

~~~~
@Is This Combination Impossible@ +=
if(uses().contains(v) && r == THESTACK)
  return false;
~~~~

If a <tt>Register</tt> is not in the <tt>inUse()</tt> set of an <tt>Instruction</tt> but the <tt>Instruction</tt> does write to it then we can't assign a <tt>Variable</tt> to that <tt>Register</tt> if it is alive after that <tt>Instruction</tt> (as we'll overwrite the value).

~~~~
@Is This Combination Impossible@ +=
if(writesTo(CAN_ERASE).contains(r))
  for(Instruction i : next())
    if(i.isLive().contains(v))
      return false;
~~~~

The final case is for the frame pointer. As we need this to resolve the memory addresses of stack-allocated values (including the <tt>Instruction</tt>s we'll add to spill <tt>Variable</tt>s to the stack) we'll keep it in a <tt>Register</tt> at all times, even across calls to other functions. Note that if this function doesn't dynamically allocate memory on the stack <tt>framePointer</tt> will be null so we'll never return false.

~~~~
@Is This Combination Impossible@ +=
if(v.equals(state.framePointer) && r == THESTACK)
  return false;
~~~~

### The LP Problem ###

Now we can start setting up the Integer Programming problem described in [the paper](http://grothoff.org/christian/lcpc2006.pdf) we're following. While we won't go into the details here, the definition of a problem centres around maximising an "objective" function subject to a series of constraints on the dependent variables in that function. These constraints are usually defined in a matrix, where each row is a different constraint, each column is a dependent variable and each cell of the matrix is the coefficient of the corresponding variable in the constraint. Each of these constraint functions must evaluate to less than or equal to a constant; the constants for each constraint are stored in a (mathematical) vector.

We'll decide on the "best" register assignment by codifying a fitness, or "objective", function for assignment. Good assignments that spill fewer variables to the stack and add fewer instructions to move variables between registers will have a lower score, and bad assignments that do expensive memory reads and writes will have a higher score. We'll define the "best" assignment as the one that produces the lowest value for the object function. As this is a Linear Programming problem the objective function must be a linear combination of the dependent variables (the columns we'll go on to define), so we'll let each column (dependent variable) decide its coefficient in the objective function:

~~~~
@Set Up LP Problem@ +=
glp_set_obj_name(allocation, "register assignment");
glp_set_obj_dir(allocation, GLP_MIN);
~~~~

We'll start by defining the columns of the constraint matrix. The (Java SWIG bindings of the) GLPK library we're using represents a problem with an instance of <tt>glp_prob</tt>. Each column in the constraint matrix is a dependent variable, and each dependent variable appears once in the objective function with a linear coefficient (which may be zero). We'll keep the logic to determine this constant in the dependent variable's <tt>ColumnKey</tt> so we only have to maintain one <tt>List</tt> of dependent variables (the <tt>columns</tt> variable). Note that as we're going to sort the columns we don't know yet what index in <tt>columns</tt> each <tt>ColumnKey</tt> will end up in, so we'll have to pass that index in when we eventually call <tt>addToObjective</tt> so we can set the right column in <tt>glp_prob</tt>'s objective function.

We referred to a <tt>updateLP</tt> method above; this is called whenever one or more <tt>Variable</tt>s or <tt>Register</tt>s change liveness or in-use state. We'll use it to clear any existing LP columns and constraints (rows) associated with the <tt>Instruction</tt> and add new ones based on the new liveness and in-use information. We could instead update the LP problem incrementally, based on exactly which <tt>Variable</tt>s or <tt>Register</tt>s have changed liveness or usage, but it's more straightforward just to make another snapshot. We'll therefore start by clearing any columns or rows we've added:

~~~~
@Instruction Members@ +=
void updateLP(ParsingState state) {
  @Remove Existing Columns and Rows@
  @Before Visiting Combinations@
  for(Variable v : isLive()) {
    @For Each Variable@
    for(Register r : Sets.complementOf(inUse()))
      if(isAssignable(v, r))
        { @For Each Combination@ }
    @After Each Variable@
  }
  @After Visiting Combinations@
}
~~~~

### Object Pools ###

GLPK also requires you to specify a whole constraint (a whole row of the constraint matrix) at once. However, we're looping over <tt>Instruction</tt>s, <tt>Variable</tt>s and <tt>Register</tt>s, and want to build up our constraints one (v, i, r) combination at a time. We'll use a static class to accumulate components of a <tt>Constraint</tt> in a pair of <tt>SWIGTYPE_p_int</tt> and <tt>SWIGTYPE_p_double</tt> columns. These are arrays stored in native (off-heap) memory and are manually allocated and freed. We'll make our <tt>Constraint</tt> class implement <tt>AutoCloseable</tt> so we can use Java 7's [try-with-resources](http://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html) statements to ensure we release the arrays' memory when we leave a block, even if an exception is thrown, so the compiler doesn't leak native memory. We don't want to allocate new native arrays for each (v, i, r) combination as that'll reduce cache locality and require more (relatively expensive) JNI calls. Instead, we'll have one instance of this class for each type of constraint, and we'll reset it every <tt>Instruction</tt>. The most number of columns a constraint will involve is <tt>numVariables * ASSIGNABLE.length</tt> (i.e. every v and r combination), though in practice a combination's contraints will need fewer columns as <tt>needsAssigning</tt> (above) will return false for some. The <tt>+ 1</tt> when choosing <tt>size</tt> is because GLPK arrays are one-indexed - we'll have to be careful of this.

~~~~
@Other Helpers@ +=
static class Row {
  final ParsingState state;
  final SWIGTYPE_p_int columns;
  final SWIGTYPE_p_double values;
  final int row;
  Row(ParsingState state, int row) {
    this.state = state;
    this.row = row;
    int size = state.numVariables * ASSIGNABLE.length + 1;
    columns = new_intArray(size + 1);
    values = new_doubleArray(size + 1);
  }
  void finalize() {
    delete_intArray(columns);
    delete_doubleArray(values);
  }
  @Row Members@
}
~~~~

The <tt>set</tt> method is the accumulator - it sets the coefficient in the current constraint to the double value passed in. As described above, it uses <tt>+ 1</tt>s to ensure the <tt>columns</tt> and <tt>values</tt> arrays are one-indexed. The <tt>len</tt> variable keeps track of how many columns we've added to the constraint function, as GLPK expects all column indices in the <tt>columns</tt> array to be valid (i.e. non-zero). We don't have to worry about GLPK keeping a reference to the array or mutating it - <tt>glp_set_mat_row</tt> just does one pass through it, copying the constraints into the <tt>glp_prob</tt>'s internal representation of the problem. <tt>addTo</tt> resets the accumulator after it adds the constraint and sets the row's fixed (<tt>GLP_FX</tt>) or lower and upper (<tt>GLP_DB</tt>) bounds. We'll make <tt>set</tt> return the <tt>Row</tt> object so it's easier to chain together several calls - another example of a [fluent interface](???).

~~~~
@Row Members@ +=
int len = 0;
Row set(int column, int value) {
  intArray_setitem(columns, len + 1, column);
  doubleArray_setitem(values, len + 1, value);
  ++len;
  return this;
}
void addTo(int lowerBound, int upperBound) {
  glp_set_mat_row(problem, row, len, columns, values);
  int type = lowerBound == upperBound ? GLP_FX : GLP_DB;
  glp_set_row_bnds(problem, row, type, lowerBound, upperBound);
}
~~~~

We need to talk a bit about how we'll manage rows in the GLPK constraint matrix. This isn't straightforwards as a row's number isn't a fixed id - it's just the current position in the matrix; delete row <tt>i</tt> and what was row <tt>i + 1</tt> is now the new row <tt>i</tt>. The problem comes from our access patterns. As we expect the problem to be long-lived (as we would keep the same one for function during an iterative compilation session) we'll constantly be adding and removing rows and columns as new <tt>Variable</tt>s become live or dead and new <tt>Register</tt>s fall into or out of use. This even happens during a one-off compilation of a function; as we create successive <tt>Instruction</tt>s as we walk through the AST we propagate liveness and <tt>Register</tt> usage backwards, revising the GLPK columns and constraints as we go. We'll therefore use an [object pool](http://en.wikipedia.org/wiki/Object_pool_pattern) to manage this row and column churn. We'll let <tt>Instruction</tt>s "create" and "delete" rows, but they'll really be checking them into or out of an object pool owned by the <tt>ParsingState</tt>. We'll only create underlying rows (via <tt>glp_add_rows</tt>) when we've got no rows outstanding.

~~~~
@Instruction Members@ +=
private final Set<Integer> rows = Sets.newTreeSet();
private Row addRow(ParsingState state) {
  Row r = state.addRow();
  rows.add(r.row);
  return r;
}
@Remove Existing Columns and Rows@ +=
for(Integer row : rows)
  state.deleteRow(row);
rows.clear();
~~~~

So what does the <tt>ParsingState</tt> side look like? We'll use a <tt>LinkedList</tt> to store the rows we've created in the GLPK problem but haven't given out to any <tt>Instruction</tt> as we'll be removing from the first element and adding to the end of the list; unlike an array-backed implementation both operations are O(1) as they just involve updating pointers. The only complexity is in <tt>deleteRow</tt> as we have to update the GLPK problem to remove whatever values were in it previously. We'll also set the target value of the constraint to zero so the constraint is now mathematically <tt>0 = 0</tt>. 

~~~~
@Parsing State Members@ +=
private final Queue<Integer> rows = Lists.newLinkedList();
Row addRow() {
  if(rows.isEmpty())
    return new Row(this, glp_add_rows(allocation, 1));
  else
    return new Row(this, rows.poll());
}
void deleteRow(int row) {
  glp_set_mat_row(allocation, row, 0, null, null);
  glp_set_row_bnds(allocation, row, GLP_FX, 0.0, 0.0);
  rows.add(row);
}
~~~~

We'll also do the same kind of object pooling for columns in the GLPK matrix. These will be more simple as we don't need to [builder](???) object:

~~~~
@Instruction Members@ +=
private final Set<Integer> cols = Sets.newTreeSet();
private int addColumn(ParsingState state) {
  int col = state.addColumn();
  cols.add(col);
  return col;
}
@Remove Existing Columns and Rows@ +=
for(Integer col : cols)
  state.deleteColumn(col);
cols.clear();
~~~~

 Deleting a column just means setting its contribution to the objective function to zero - so no matter what the value ends up as it won't affect which solution we pick as optimal.

~~~~
@Parsing State Members@ +=
private final Queue<Integer> cols = Lists.newLinkedList();
int addColumn() {
  if(cols.isEmpty())
    return glp_add_cols(allocation, 1));
  else
    return cols.poll();
}
void deleteColumn(int col) {
  glp_set_obj_coef(allocation, col, 0.0);
  cols.add(col);
}
~~~~

As <tt>updateLP</tt> iterates over <tt>Variable</tt>s in its outer loop and <tt>Register</tt>s it its inner loop, we need one further class to make managing one constraint per <tt>Register</tt> (possibly encompassing all <tt>Variable</tt>s) easier. We'll use a [composite](???) class, <tt>Rows</tt>, to wrap the <tt>Row</tt> class:

~~~~
@Other Helpers@ +=
static class Rows {
  final Row[] rows = new Row[Register.values().length];
  final ParsingState state;
  Rows(ParsingState state) {
    this.state = state;
  }
  void set(Register r, int column, int value) {
    Row row = rows[r.ordinal()];
    if(row == null)
      row = rows[r.ordinal()] = addRow(state);
    row.set(column, value);
  }
  void addTo(int lowerBound, int upperBound) {
    for(Row row : rows)
      if(row != null)
        row.addTo(allocation, lowerBound, upperBound);
  }
}
~~~~

### Variable-In-Register Constraints ###

The first (and most important) set of dependent variables are the <tt>varInReg</tt> columns; these represent the variable <tt>v</tt> being in <tt>Register</tt> <tt>r</tt> at <tt>Instruction</tt> <tt>i</tt>, and so store the solution of the register assignment problem. We're phrasing this as an Integer programming problem, so the <tt>varInReg</tt> variables can only take integer values, and we'll add extra constaints to ensure the value is either zero or one. We can then intepret the zero or one values as a binary yes-or-no output. We'll use the <tt>needsAssigning</tt> method we defined earlier so we only have dependent variables for valid combinations - the fewer dependent variables our Integer Programming problem has the faster it will be to solve it. We'll define the <tt>addToObjective</tt>'s <tt>coefficient</tt> later as this is the main way we'll modify the problem to encode more domain knowledge. We want the optimal solution (the solution that minimises the objective function) to generate the most efficient solution, so we want to penalise register assignments that require expensive operations (like writing to the stack).

~~~~
@For Each Combination@ +=
int varInReg = addColumn(state);
double coefficient = 0d;
@Determine VarInReg Coefficient@
glp_set_obj_coef(state.allocation, varInReg, coefficient);
~~~~

How should we decide what value to add to the objective function if a particular (v, i, r) combination is chosen? While we know what assignments we want to penalise or reward, the amount they add to the objective function (and so their relative importance) is not quite so clear. We'll pick values rather arbitarily for now; we should tweak these values after we've seen examples of the assignments they produce.

The first behaviour we want to penalise is spills to the stack. This isn't as simple as increasing the objective function each time a variable is assigned to the <tt>THESTACK</tt> dummy register. For example, what if a variable was live but not used at instructions 3, 4 and 5, and had to be spilled at instructions 3 and 5 due to memory pressure. We wouldn't want to reward an assignment that moved the variable back to a register for instruction 4, only to spill it again for instruction 5. We'd prefer an assignment that spilled it once at instruction 3 and kept it on the stack for instructions 4 and 5. We need a way to penalise moves between the stack and registers, and looking at and assignment to the stack for a single instruction doesn't give us a good enough way to do this.

However, we know that if a variable is used (read or written) by an instruction that variable must be assigned to a register (i.e. can't be on the stack). Therefore, if the variable is on the stack for an instruction that needs it we'll have to insert a expensive <tt>mov</tt> instruction to load it from the stack - so we can penalise this behaviour:

~~~~
@Other Helpers@ +=
private static final double COST_OF_SPILL = 100.0;
@Determine varInReg Coefficient@ +=
if(r == THESTACK && uses().contains(v))
  coefficient += COST_OF_SPILL;
~~~~

We need another flag if we want to penalise a spill to the stack. We'll use constraints to ensure that the <tt>NEW_VAR_IN_REG_FLAG</tt> dependent variables have a one for any <tt>(v, r, i)</tt> combination if register <tt>r</tt> had a different variable in before instruction <tt>i</tt> and contains variable <tt>v</tt>, and in all other circumstances the constraints should force the <tt>NEW_VAR_IN_REG_FLAG</tt> flag to zero. This dependent variable will be an important part of the objective function as we want to penalise assignments for each <tt>NEW_VAR_IN_REG_FLAG</tt> they have set (as even <tt>mov</tt>s between registers have some cost) - but especially if the register <rr>r</tt> is the dummy <tt>THESTACK</tt> register.

The cost of register access, <tt>mu</tt> in [the paper](http://grothoff.org/christian/lcpc2006.pdf), can be a function of the register and the instruction. This gives us the option of discouraging moves in frequently-used instructions (such as those in the body of a loop). However, for the minute we'll only give a flat objective function penalty of <tt>+1.0</tt> for register moves and <tt>+100.0</tt> for stack moves.

~~~~
@For Each Combination@ +=
int newVarInReg = addColumn(allocation);
glp_set_obj_coef(allocation, newVarInReg, 
  r == THESTACK ? COST_OF_STACK_ACCESS : COST_OF_REG_ACCESS);
@Other Helpers@ +=
private static final double COST_OF_REG_ACCESS = 1.0;
private static final double COST_OF_STACK_ACCESS = 100.0;
~~~~

The objective function as we've defined it so far will give us an efficient allocation. However, there's some domain knowledge we can take advantage of - and we'll do this by adding 'hints' to the objective function. We'll subtract a small amount from the objective function if an allocation satisfies our hint (and so the Integer Programming solver will choose an allocation that follows the hints over one that doesn't). However, these hints dwarfed by the penalties we've defined, so won't come at the cost of additional memory accesses.

Our first hint aims to make as many <tt>mov</tt>s instructions no-ops as possible (especially if the <tt>Instruction</tt> contains a fixed <tt>Register</tt>). For example, as we insert a <tt>mov</tt> just before a <tt>ret</tt> to set the <tt>rax</tt> register to the return value; ideally the return value would already be in <tt>rax</tt> so we could save an instruction. Similarly, after a <tt>Call</tt> <tt>Instruction</tt> we add a <tt>mov</tt> to put the value the function returns in <tt>rax</tt> to the return <tt>Variable</tt> defined in the programme.

~~~~
@Instruction Members@ +=
boolean couldBeNoOp(Variable v, Register r) { return false; }
@Move Members@ +=
boolean couldBeNoOp(Variable v, Register r) { return uses(v) && uses(r); }
@Determine varInReg Coefficient@ +=
if(couldBeNoOp(v, r))
  coefficient += MOVE_NOP_HINT;
@Other Helpers@ +=
private static final double MOVE_NOP_HINT = -1.0;
~~~~

### Aliasing Variables ###

Two variables are 'aliased' at an <tt>Instruction</tt> if that <tt>Instruction</tt> makes one of them dead (i.e. <tt>isLive()</tt> doesn't contain them for each following <tt>Instruction</tt>) and makes the other live (i.e. <tt>isLive()</tt> of at least one following <tt>Instruction</tt> contains it - remember <tt>isLive</tt> describes the variable's state just _before_ the <tt>Instruction</tt> is executed). Normally we'll use constraints to only allow one variable in a register at once (as one of their values would be erased), but if we assigned two aliasing variables to the same <tt>Register</tt> at an <tt>Instruction</tt> we would't lose information - as we don't mind losing the value of the <tt>Variable</tt> that just became 'dead'. We'll generate all the pairs of <tt>Variable</tt>s that could be put in the same <tt>Register</tt> (we're not generalising to sets of _n_ <tt>Variable</tt>s that could go in the same <tt>Register</tt> as the assembly instructions we'll generate only have at most two arguments). We're calculating the <tt>Variable</tt>s that are first made live by this <tt>Instruction</tt> (are "born") using the difference between <tt>isLive</tt> and <tt>isLive()</tt> (i.e. the <tt>Variable</tt>s referred to by the current <tt>Instruction</tt> but are not live). We have to be careful not to refer to the <tt>Instruction</tt>s following this one (in <tt>prev()</tt>) as then we'd risk missing aliasing pairs created when a successor <tt>Instruction</tt> changed but the current one didn't, as then we wouldn't reconstruct the GLPK columns and constraints for this <tt>Instruction</tt>.

~~~~
@Instruction Members@ +=
Set<Variable> born() {
  return Sets.difference(isLive(), isLive);
}
@Before Visiting Combinations@ +=
Map<Pair<Variable, Variable, Rows> aliasing = Maps.newHashMap();
for(Variable a : onlyVariables(killed()))
  for(Variable b : born())
    aliasing.put(Pair.of(a, b), new Rows(state));
~~~~

We'll therefore force the solver to assign aliasing <tt>Variable</tt>s together by adding a constraint for each <tt>Register</tt> that makes one <tt>Variable</tt>'s <tt>varInReg</tt> minus the other's equal to zero, so for that <tt>Register</tt> either both <tt>varInReg</tt>s are one or both are zero. This means the ordering of the <tt>Variable</tt>s in the aliasing <tt>Pair</tt> (i.e. does the killed <tt>Variable</tt> come before the new one?) doesn't matter.

~~~~
@For Each Combination@ +=
for(Entry<Pair<Variable, Variable, Rows> e : aliasing) {
  if(v.equals(e.getKey().getLeft()))
    rows.set(r, varInReg, 1);
  else if(v.equals(e.getKey().getRight()))
    rows.set(r, varInReg, -1);
}
@After Visiting Combinations@ +=
for(Rows rows : aliasing.values())
  rows.addTo(allocation, 0, 0);
~~~~

### One Variable Per Register ###

Now that we've identified any aliasing <tt>Variable</tt>s we can add a constraint to enforce that a single <tt>Register</tt> (but not the stack - we can store any number of <tt>Variable</tt>s there) can only have a single <tt>Variable</tt>, or a pair of aliasing <tt>Variable</tt>s assigned to it at each <tt>Instruction</tt>. We'll do this by ensuring that the sum of the <tt>varInReg</tt>s for each <tt>Variable</tt> assigned to that <tt>Register</tt> is between zero and one (the <tt>Register</tt> could be empty if there are fewer live <tt>Variable</tt>s than there are <tt>Register</tt>s). We'll implement this by skipping (arbitrarily) the second <tt>Variable</tt> of each aliasing pair, so each pair of aliasing <tt>Variable</tt>s only contributes one <tt>varInReg</tt> to the contraint's sum, and so we capping the sum at one means limiting the number of assigned <tt>Variable</tt>s to one. Note that we'd have to do this slightly differently if we gave the GLPK solver the choice of whether to place aliasing <tt>Variable</tt>s in the same <tt>Register</tt> or not, as these constriants below allow the second <tt>Variable</tt> of each aliasing pair to go in any <tt>Register</tt>, regardless of whether it's occupied or not.

~~~~
@Before Visiting Combinations@ +=
Rows oneVarPerReg = new Rows(state);
@For Each Combination@ +=
boolean relevant = r != THESTACK;
for(Pair<Variable, Variable> p : aliasing.keySet())
  relevant &= !v.equals(p.getRight());
if(relevant)
  oneVarPerReg.set(r, varInReg, 1);
@After Visiting Combinations@ +=
oneVarPerReg.addTo(0, 1);
~~~~

### Ensuring Variables Aren't Lost ###

We also want to ensure the (live) <tt>Variable</tt>s aren't 'lost' - they must be assigned to exactly one <tt>Register</tt> (which includes <tt>THESTACK</tt>). We'll make the constraint _exactly_ one rather than _at least_ one as otherwise we'd have to do extra work to ensure all copies of a <tt>Variable</tt> were updated when one was. As we loop over <tt>Variable</tt>s then <tt>Register</tt>s this step relatively straightforward:

~~~~
@For Each Variable@ +=
Row varsNotLost = addRow(state);
@For Each Combination@ +=
varsNotLost.set(varInReg, 1);
@After Each Variable@ +=
varsNotLost.addTo(1, 1);
~~~~

### Setting the "New Variable In Register" Flag ###

The last set of constraints ensure that the <tt>newVarInReg</tt> flags have the semantics we want: for each <tt>Variable</tt> and <tt>Register</tt> combination, when this column has a `1` the <tt>Variable</tt> either became live for the first time (i.e. first appeared in <tt>isLive()</tt>) or was assigned to a different <tt>Register</tt> at any one of the <tt>prev()</tt> <tt>Instruction</tt>s. The case when a <tt>Variable</tt> dies (i.e. is in <tt>isLive()</tt> at the <tt>Instruction</tt> but not in any of the <tt>next()</tt> <tt>Instruction</tt>s) is already covered; the column won't exit in the <tt>next()</tt> <tt>Instruction</tt>s so we don't have to worry about setting it.

This first case is easy: to force the <tt>newVarInReg</tt> flag to one we'll add a constraint of the form <tt>1 * newVarInReg == 1</tt>. We'll use the "<tt>born()</tt>" <tt>Set</tt> of newly-live <tt>Variable</tt>s we calculated earlier.

~~~~
@For Each Combination@ +=
if(born().contains(v)) {
  addRow(state).set(newVarInReg, 1).addTo(1, 1);
} else {
  @Calculate newVarInReg From Previous@
}
~~~~

The second case - if the <tt>Variable</tt> was live at at least one <tt>prev()</tt> <tt>Instruction</tt> - is a bit more difficult. The first thing we can definitely say is that, at each combination, if the <tt>Variable</tt>'s not actually in that <tt>Register</tt> then the the <tt>newVarInReg</tt> flag must be zero. We can enforce this with a constraint of the form <tt>0 <= 1 * varInReg - 1 * newVarInReg <= 1</tt>. That is, if the <tt>Variable</tt> is in the <tt>Register</tt> at this <tt>Instruction</tt> (<tt>varInReg == 1</tt>) this reduces to <tt>0 <= 1 - newVarInReg <= 1</tt>, and so <tt>newVarInReg</tt> can be either <tt>0</tt> or <tt>1</tt>. However, if the <tt>Variable</tt> is assigned to a different <tt>Register</tt> (i.e. not the one in the currrent combination, <tt>varInReg == 0</tt>) then the constraint reduces to <tt>0 <= - 1 * newVarInReg <= 1</tt>, so <tt>newVarInReg</tt> must be <tt>0</tt>.

~~~~
@Calculate newVarInReg From Previous@ +=
addRow(state).set(varInReg, 1).set(newVarInReg, -1).addTo(0, 1);
~~~~

If we couldn't possibly put the <tt>Variable</tt> in the <tt>Register</tt> last time as it wasn't available (it's in of the <tt>prev()</tt> <tt>Instruction</tt>'s <tt>inUse()</tt> sets), yet it's now in the <tt>Register</tt> at this <tt>Instruction</tt>, then the <tt>newVarInReg</tt> flag must be true. We'll enforce this mathematically with a contraint similar to the previous one: <tt>varInReg - newVarInReg == 0</tt>. If <tt>varInReg</tt> is <tt>1</tt>, then <tt>newVarInReg</tt> must be <tt>1</tt> for the difference between the two to be zero, and similarly if <tt>varInReg</tt> is <tt>0</tt>.

~~~~
@Calculate newVarInReg From Previous@ +=
for(Instruction i : prev())
  if(prev().inUse().contains(r)) {
    addRow(state).set(varInReg, 1).set(newVarInReg, -1).addTo(0, 0);
    break;
  }
~~~~

We need one more bit of information to add the remaining constraints: we need to know whether the combination's <tt>Variable</tt> was in the <tt>Register</tt> (i.e. the value of the <tt>varInReg</tt> column) at any of the <tt>prev()</tt> <tt>Instruction</tt>s. This means we'll have to make each <tt>Instruction</tt> store the <tt>varInReg</tt> column numbers for each of it's combinations. We'll make this information available via a <tt>varInRegCol</tt> method on the <tt>Instruction</tt>, and we'll rely on other <tt>Instruction</tt>s only calling this method with valid arguments.

~~~~
@Instruction Members@ +=
final Map<Pair<Variable, Register>, Integer> varInRegCols = Maps.newHashMap();
Integer varInRegCol(Variable v, Register r) {
  return varInRegCols.get(Pair.of(v, r));
}
@For Each Combination@ +=
varInRegCols.put(Pair.of(v, r), varInReg);
@Remove Existing Columns and Rows@ +=
varInRegCols.clear();
~~~~

We must also ensure that, for *all* of the previous <tt>Instruction</tt>s, if the combination's <tt>Variable</tt> was in this <tt>Register</tt> at that <tt>Instruction</tt> and this one then the <tt>newVarInReg</tt> flag must be zero. I mean *all*, not any, as otherwise (i.e. if the flag is true if *any* of the predecessors have the <tt>Variable</tt> in that <tt>Register</tt>) then we won't penalise this assignment in the objective function, so the compiler would essentially get a <tt>mov</tt> instruction (of the <tt>Variable</tt> between the two <tt>Register</tt>s) for free. Luckily we don't have to explicitly add any constraints to enforce this, as the solver will always picking a solution where <tt>newVarInReg == 0</tt> when given the choice, as that will result in a lower value of the objective function (which the solver is trying to minimise).

This leaves our final (and most complicated) constraint. We'll consider each of the <tt>Instruction</tt>'s predecessors in turn, and if we see that the <tt>Variable</tt> was not in the current combination's <tt>Register</tt> then, but is now, then we'll set the <tt>newVarInReg</tt> flag to one. We'll do this by ensuring <tt>varInReg[prev] - varInReg + newVarInReg >= 0</tt> (where <tt>varInReg[prev]</tt> is the value of the <tt>varInReg</tt> column for this combination at the previous <tt>Instruction</tt>). The table below shows how this constraint restricts the value of <tt>newVarInReg</tt> (where <tt>?</tt> means the value isn't restricted):

|| <tt>varInReg[prev]</tt> || <tt>varInReg</tt> || <tt>newVarInReg</tt> ||
| 0 | 0 | ? |
| 1 | 0 | ? |
| 0 | 1 | 1 |
| 1 | 1 | ? |

As you can see, this only restricts a single case: when the <tt>Variable</tt> wasn't in the <tt>Register</tt> last <tt>Instruction</tt> but is now - and in this case the <tt>newVarInReg</tt> flag is forced to one. As we mentioned above, if the solver gets to choose the value of <tt>newVarInReg</tt> it'll always choose <tt>0</tt>, so if there's only a single predecessor <tt>Instruction</tt> you could consider all the <tt>?</tt>s in the table as zeros.

~~~~
@Calculate newVarInReg From Previous@ +=
for(Instruction i : prev()) {
  Integer c = i.varInRegCol(v, r);
  if(c != null)
    addRow(state).
      set(c, 1).
      set(varInReg, -1).
      set(newVarInReg, 1).
      addTo(0, 2);
}
~~~~

### Solving The Problem ###

We can now feed the problem into the GLPK solver. The [GLPK manual](http://www.mai.liu.se/~kahol/kurser/all/glpk.pdf) will be useful here, as will [a list of all the GLPK options](http://www.maximal-usa.com/solvopt/optglpk.html). We want to use the [branch and cut method](http://homepages.rpi.edu/~mitchj/handouts/bc_eg) (<tt>glp_intopt</tt> in GLPK). We also want to turn on the pre-solver (<tt>setPresolve(GLP_ON)</tt>). This basically tells the solver to try transforming the problem into a Mixed-Integer Programming problem (one where some variables we've specified as integers are allowed to be real numbers) in the hope that this variant is easier to solve; it'll then transform the optimal solution it finds to the MIP problem to an optimal solution of the Integer Programming problem we've specified.

~~~~
@Parsing State Members@ +=
glp_iocp iocp = new glp_iocp();
@Set Up LP Problem@ +=
glp_init_iocp(iocp);
iocp.setPresolve(GLP_ON);
@Solve@ +=
if (glp_intopt(state.allocation, state.iocp) != 0)
  throw new IllegalStateException();
~~~~

### Unpacking The Results ###

We know which register each variable is in at each node now (assuming it's live):

~~~~
@Other Helpers@ +=
static Register whereAmI(glp_prob allocation, ControlFlowGraph controlFlow, int v, int n) {
  for(Register r : ASSIGNABLE) {
    int index = Collections.binarySearch(controlFlow.columns, new varInReg(v, r, n));
    if(index >= 0 && glp_mip_col_val(allocation, index + 1) > 0.5)
        return r;
  }
  throw new IllegalStateException(); // shouldn't get here
}
~~~~

Now we know where to put variables, we can update the instructions. We'll make the instructions immutable, and so we need copy-on-resolve semantics.

~~~~
@Instruction Members@ +=
Instruction rewrite(Map<Value, Value> resolutions) { return this; }
@BinaryOp Members@ +=
Instruction rewrite(Map<Value, Value> resolutions) { 
  return new BinaryOp(op, resolve(arg1, resolutions), resolve(arg2, resolutions)); 
}
@Push Members@ += 
Instruction rewrite(Map<Value, Value> resolutions) {
  return new Push(resolve(value, resolutions));
}
@Move Members@ +=
Instruction rewrite(Map<Value, Value> resolutions) {
  return new Move(resolve(from, resolutions), resolve(to, resolutions), cond); 
}
~~~~

~~~~
@Static Classes@ +=
interface Resolvable {
  StorableValue rewrite(Map<Value, Value> resolutions);
}
@At Address Members@ +=
public StorableValue rewrite(Map<Value, Value> resolutions) { 
  Value resolution = resolutions.get(at);
  return resolution == null ? this : new AtAddress((StorableValue) resolution, offset); 
}
@Static Parameter Members@ +=
public StorableValue rewrite(Map<Value, Value> resolutions) { return this; } 
@Other Helpers@ +=
protected static <E> E resolve(E from, Map<Value, Value> resolutions) {
  if(from instanceof Resolvable)
    return (E) ((Resolvable) from).rewrite(resolutions);
  else {
    Value resolution = resolutions.get(from);
    return (E) (resolution == null ? from : resolution);
  }
}
~~~~

We'll iterate through the linear programming solution and resolve variables as needed. For each instruction, we'll use the node before to work out which variables are where. Note that the variable must be live at the previous node, by definition.

~~~~
@Copy Solution Into Code@ +=
for(int i = 0; i < state.code.size(); ++i) {
  Map<Value, Value> resolutions = Maps.newHashMap();
  for(Variable variable : state.code.get(i).uses(Variable.class)) {
    int v = indexOf(controlFlow.variables, variable);
    for (Register r : ASSIGNABLE) {
      int index = Collections.binarySearch(controlFlow.columns, new varInReg(v, r, controlFlow.prevNode[i]));
      if(index >= 0 && glp_mip_col_val(allocation, index + 1) > 0.5) // huge tolerance for 0-1 integer
        resolutions.put(variable, r);
    }
  } 
  state.code.set(i, state.code.get(i).rewrite(resolutions));
}
~~~~

~~~~
@Calculate Stack Moves Needed@ +=
Multimap<Integer, Instruction> stackMovesAfter = HashMultimap.create();
for (int v = 0; v < controlFlow.numVariables; ++v)
  for (int n = 0; n < controlFlow.numNodes; ++n)
    for(Register r : ASSIGNABLE) {
      int index = Collections.binarySearch(controlFlow.columns, new NewVarInRegFlag(v, r, n));
      if(index >= 0 && glp_mip_col_val(allocation, index + 1) > 0.5)
        for(int i : controlFlow.instructionsBeforeNode.get(n)) {
          @V Has Moved@
          if(whereAmI == Register.THESTACK)
            { @V Copied From Register To Stack@ }
          else if(whereWasI == THESTACK)
            { @V Copied From Stack To Register@ }
          else
            { @V Copied From Register To Register@ }
        }
    }
~~~~


~~~~
@V Has Moved@ +=
Register whereAmI = r;
Register whereWasI = whereAmI(allocation, controlFlow, v, controlFlow.prevNode[i]);
~~~~

~~~~
@Find Where I Live On The Stack@ +=
Supplier<Long> stackOffset = state.spillVariable(controlFlow.v);
~~~~

Who knows where the stack pointer is? If it's not in a fixed register, check the GLPK solution:

~~~~
@Parsing State Members@ +=
Register stackVarsRelativeTo(glp_prob allocation, ControlFlowGraph controlFlow, int n) {
  if(framePointer == null)
    return rsp;
  else
    return whereAmI(allocation, controlFlow, indexOf(controlFlow.variables, framePointer), n);
}
~~~~

The != _THESTACK_ check catches newly-initialised variables and dead variables (which are forced onto the stack).

~~~~
@V Copied From Register To Stack@ +=
@Find Where I Live On The Stack@
stackMovesAfter.put(i, move(
  whereWasI, 
  new AtAddress(
    state.stackVarsRelativeTo(allocation, controlFlow, controlFlow.prevNode[i]), 
    stackOffset)));
~~~~

The question is, where was V? On the stack or in a register?

~~~~
@V Copied From Stack To Register@ +=
@Find Where I Live On The Stack@
stackMovesAfter.put(i, move(
  new AtAddress(
    state.stackVarsRelativeTo(allocation, controlFlow, controlFlow.prevNode[i]),
    stackOffset),
  whereAmI));
~~~~

~~~~
@V Copied From Register To Register@ +=
stackMovesAfter.put(i, move(whereWasI, whereAmI));
~~~~

Inserting is a bit tricky as the indices will change. We need a treeset as we walk through the code array, so need the insertions in sequence.

~~~~
@Insert Stack Moves@ +=
int drift = 0;
for(int i : Sets.newTreeSet(stackMovesAfter.keySet())) {
  Set<Instruction> toAdd = Sets.newHashSet(stackMovesAfter.get(i));
  List<Instruction> toAddReordered = Lists.newArrayList();
  @Choose Stack Move Order@
  state.code.addAll(i + drift + 1, toAddReordered); 
  drift += toAddReordered.size();
}
~~~~

The order is important, if we want to avoid flattening values moved from register to register with those moved from the stack. The stack pointer's especially important, as we'll either wnat to use it to store others before storing it, or load it before loading others relative to it. We basically have to create a dependency tree, and do a topological sort to perform the moves in a non-overlapping manner.

~~~~
@Choose Stack Move Order@ +=
@Calculate Move Dependencies@
@Reorder Stack Moves@
~~~~

MemoryAddresses use the location of the stack pointer after the stack moves, but all other references use the stack pointer location before stack moves. We have to undo this when moving things about.

The dependencies map is "key must come after all values" so we can pick a random key even if the map is a forest. Let's pick the deadlocker as the one that does the writing (as we know that's a simple move, so cheap to convert to a swap, as it doesn't touch the ram).

~~~~
@Calculate Move Dependencies@ +=
Multimap<Instruction, Instruction> dependencies = HashMultimap.create();
Instruction deadlocker = null;
while((deadlocker = calculateDependencies(dependencies, toAdd)) != null) {
  @Resolve Current Deadlock@
}
@Other Helpers@ +=
static Instruction calculateDependencies(
  Multimap<Instruction, Instruction> dependencies,
  Iterable<Instruction> toAdd) {
  for(Instruction a : toAdd)
    for(Instruction b : toAdd) {
      if(a == b) continue;
      @Add Dependencies@
    }
  return null;
}
~~~~

We loop over all instructions, so we'll see each pair both ways round (e.g a,b and b,a). 

We very much want to avoid swapping with a memory address, as that has expensive locking semantics (http://en.wikibooks.org/wiki/X86_Assembly/Data_Transfer#Data_Swap)

~~~~
@Add Dependencies@ +=
if(!Sets.intersection(a.readsFrom(Register.class), b.writesTo(Register.class, CAN_ERASE)).isEmpty()) {
  dependencies.put(b, a); // b must be after a
  if(dependencies.get(a).contains(b)) 
    return b.uses(AtAddress.class).isEmpty() ? b : a; // cheapest 
}
~~~~

We can break deadlocks using the XCHG instruction to swap two registers or a memory location and a register. The latter is very expensive, so try and switch registers wherever possible! To clarify, writesTo means destructively write to, so a swap returns none(..) here as it doesn't destroy either input.

~~~~
@Static Classes@ +=
static final class Swap extends Instruction {
  final StorableValue arg1; final StorableValue arg2; 
  Swap(StorableValue arg1, StorableValue arg2) { this.arg1 = arg1; this.arg2 = arg2; }
  public String toString() { @Swap x86 Code@ }
  @Swap Members@
}
@Move Members@ +=
Instruction toSwap() { return new Swap((StorableValue) from, to); }
~~~~

Also, all the instructions are unconditional moves (unless they've been replaced with swaps) - so in any case the only way they write to a register is if the to-Value of the move is not a MemoryAddress.

~~~~
@Resolve Current Deadlock@ +=
toAdd.remove(deadlocker);
toAdd.add(((Move)deadlocker).toSwap());
dependencies.clear();
~~~~

If we've inserted any swaps we'll have to re-write the following instructions, as they (currently) assume simultanious execution.

~~~~
@Propagate Swap Effects@ +=
Map<Value, Value> swaps = Maps.newHashMap();
for(int j = 0; j < toAddReordered.size(); ++j) {
  if(toAddReordered.get(j) instanceof Swap) {
    Swap swap = (Swap) toAddReordered.get(j);
    toAddReordered.set(j, new Swap(resolve(swap.arg1, swaps), swap.arg2));
    swaps.put(swap.arg1, swap.arg2);
    swaps.put(swap.arg2, swap.arg1);
  } else {
    Move move = (Move) toAddReordered.get(j);
    toAddReordered.set(j, move(resolve(move.from, swaps), move.to));
  }
}
~~~~

Picking next isn't trivial - if some instructions load from the stack they'll need the framepointer, so if we're moving that (e.g. from a callee-saved register to somewhere else) then move it first. If not, the memory load will expect the frame pointer to be in a non-callee saved register!

~~~~
@Reorder Stack Moves@ +=
while(!toAdd.isEmpty()) {
  Instruction next = Iterables.get(toAdd, 0);
  topologicalRemove(next, toAdd, toAddReordered, dependencies);
}
@Propagate Swap Effects@
@Other Helpers@ +=
static <I> void topologicalRemove(
  I i, 
  Set<I> remaining, 
  Collection<I> inOrder,
  Multimap<I, I> dependencies) {
  if(!remaining.remove(i)) return;
  for(I dependency: dependencies.get(i))
    topologicalRemove(dependency, remaining, inOrder, dependencies);  
  inOrder.add(i);
}  
~~~~

## (III) Writing the Output ##

We'll now write out all the <tt>Instruction</tt>s we've generated in AT&T syntax. You can compile the generated Assembly code with [GNU's "as"](http://tigcc.ticalc.org/doc/gnuasm.html) ([OSX-specific documentation](https://developer.apple.com/library/mac/#documentation/DeveloperTools/Reference/Assembler/000-Introduction/introduction.html)), [yasm](http://yasm.tortall.net) or LLVM's assembler, ["llvm-mc"](???).

~~~~
@Write The Output@ +=
@Set Source Format@
@Populate Bss And Data Sections@
@Append Instructions@
~~~~

We'll fill in all the <tt>toString</tt> methods of the <tt>Instruction</tt> subclasses, starting with <tt>Definition</tt> and its subclass <tt>FunctionDefinition</tt>. If a the <tt>Label</tt>'s a function definition and is exported we'll insert a <tt>.globl</tt> directive to tell the assembler to flag it in the symbol table it builds up.

~~~~
@Definition x86 Code@ +=
return label.name + ':';
@Head x86 Code@ +=
if(isExported)
  return String.format(".globl %s\n%s:", label.name, label.name);
else
  return label.name + ':';
~~~~

Note that this generates the same Assembly code for the symbol definition marking the beginning of a function and an label just used for control flow within a function. Assemblers can embed debug information in the generated binary to help debuggers discriminate between the two use cases (among other things). The [various formats](http://dwarfstd.org/doc/Debugging%20using%20DWARF-2012.pdf) use different methods to embed this information; _stabs_ adds extra symbols to the binary's symbol table while _dwarf_ adds all the debug information to a different section of the executable (usually with section names beginning with <tt>.debug_</tt>). The assembler expects different [directives](http://cs.mtu.edu/~mmkoksal/blog/?x=entry:entry120116-130037) for the different debug formats; COFF uses a variety of directives inside a <tt>.def</tt> block, dwarf uses a [set of directives](http://www.logix.cz/michal/devel/gas-cfi) beginning with <tt>.cfi_</tt> for stack frame tracking and a [complex](http://stackoverflow.com/questions/14422229/basic-os-x-assembly-and-the-mach-o-format) tree describing the assembly code in debug sections of the binary and stabs uses <tt>.stabs</tt> [and other](http://www.chemie.fu-berlin.de/chemnet/use/info/gas/gas_7.html#SEC122) directives.

We won't add any debug info to the Assembly code we generate as assembler support for the various directives is quite patchy. While most assemblers support <tt>.file</tt> and <tt>.line</tt> directives, dwarf debugging information extends these to add a file number argument to <tt>.file</tt> (so one Assembly file can have code from a variety of input files scattered throught) and adds a <tt>.loc</tt> directive instead of <tt>.line</tt>, which can have the file and column numbers in addition to the line number as arguments.

Most other <tt>Instruction</tt>s and <tt>Value</tt>s are straightforward:

~~~~
@BinaryOp x86 Code@ += return String.format( "%s %s, %s", op, arg1, arg2);
@Push x86 Code@ += return String.format( "pushq %s", value);
@Pop x86 Code@ += return String.format( "popq %s", value);
@Call x86 Code@ +=
if(name instanceof Label)
  return String.format( "call %s", ((Label)name).name); // call a label
else
  return String.format( "call *%s", name); // indirect function call - code address in a register
@Ret x86 Code@ += "ret"
@Immediate x86 Code@ += return String.format("$0x%X", value.get());
@Jump x86 Code@ += return String.format("j%s %s", cond == null ? "mp" : cond, to.label.name);
@Move x86 Code@ += 
if(cond == null)
  return String.format("movq %s, %s", from, to);
else
  return String.format("cmov%s %s, %s", cond, from, to);
@Swap x86 Code@ += return String.format("xchg %s, %s", arg1, arg2);
~~~~

<tt>Label</tt>s provide the only interesting cases as we want to generate [position independent code](http://en.wikipedia.org/wiki/Position-independent_code). x86-64 processors [implement this](http://eli.thegreenplace.net/2011/11/11/position-independent-code-pic-in-shared-libraries-on-x64) by defining the memory addresses that labels point to as relative offsets to the instruction pointer. This relative address actually points into the [Global Offset Table](http://bottomupcs.sourceforge.net/csbu/x3824.htm); when the binary is loaded into memory at runtime the loader puts the actual memory address that the label points to in this GOT entry. This means if we want to load a value from a label we have to do two memory loads; one to get the real location from the GOT and the second to get the value from the real location.

~~~~
@Label x86 Code@ += return String.format("%s@GOTPCREL(%%rip)", name);
@At Address x86 Code@ +=
if(at instanceof Label)
  return String.format("%s(%%rip)", ((Label)at).name);
else if(offset.get() == 0)
  return String.format("(%s)", at);
else
  return String.format("%s0x%X(%s)", offset.get() < 0 ? "-" : "", Math.abs(offset.get()), at);
~~~~

We'll also tell the assembler that we're only generating 64-bit assembly code:

~~~~
@Set Source Format@ +=
asmOut.append(".code64\n");
~~~~

### Data Sections ###

We'll represent the memory locations that the exported and un-exported <tt>Label</tt>s point to as another subclass of <tt>Instruction</tt>. The initial value - the value we'll embed in the binary file - is the <tt>Immediate</tt> from the <tt>globals</tt> field of the <tt>ProgramState</tt>.

~~~~
@Static Classes@ +=
static final class DefineLong extends Instruction {
  final long initialValue;
  DefineLong(Immediate initialValue) {
    this.initialValue = initialValue.value.get();
  }
  public String toString() { @Define Long x86 Code@ }
}
~~~~

We'll also use a static class to represent the start of each segment.

~~~~
@Static Classes@ +=
static final class Segment extends Instruction {
  final String segment;
  Segment(String segment) {
    this.segment = segment;
  }
  public String toString() { @Segment x86 Code@ }
}
~~~~

The <tt>.data</tt> and <tt>[.bss](http://en.wikipedia.org/wiki/.bss)</tt> segments of the Assembly code we output have read and write, but not execute, permissions. We'll store our exported and unexported variables here, although we'll make no attempt to group the variables in each segment either by whether or not they're exported or by the patterns of read and write accesses to them. As space for the values stored in the <tt>.bss</tt> segment isn't allocated in the binary file the assembler creates, when the loader copies the binary file into memory it "inflates" the binary file, allocating zero-initialised memory for each variable in the <tt>.bss</tt> segment. Values in the <tt>.data</tt> segment are stored in the binary file, so the loader just copies these verbatim into memory. This means we'll divide the <tt>Label</tt>s in the <tt>global</tt> map between the <tt>.bss</tt> and <tt>.data</tt> sections depending on whether the initial value of the symbol (the <tt>Immediate</tt> in the map) is zero.

We'll store the bss & data <tt>Instruction</tt>s in local variables; <tt>data</tt> and <tt>bss</tt> contain the head of each segment's intrinsic linked list and <tt>dataTail</tt> and <tt>bssTail</tt> store the current ends of the lists.

~~~~
@Populate Bss And Data Sections@ +=
Instruction
  bss = new Segment("bss"),
  data = new Segment("data");
Instruction bssTail = bss, dataTail = data;
for(Entry<Label, Immediate> global : program.globals.entrySet()) {
  Definition def = new Definition(
    global.getKey(), 
    global.getKey().isUpperCase());
  DefineLong value = new DefineLong(global.getValue());
  if(value.initialValue == 0)
    bssTail = bssTail.append(def, null).append(value, null);
  else
    dataTail = dataTail.append(def, null).append(value, null);
}
~~~~

If we're putting a <tt>DefineLong</tt> in the <tt>.bss</tt> section we only need to use a <tt>.space</tt> directive, as the inital value will always be zero. We could have put <tt>.long 0x0</tt>, but different directives emphasise the different semantics of the <tt>.data</tt> and <tt>.bss</tt> segments.

~~~~
@Define Long x86 Code@ += 
if(initialValue == 0)
  return String.format(".space 0x%X", SIZE);
else
  return String.format(".long 0x%X", initialValue);
~~~~

### No-Ops ###

When we ran the register allocation algorithm in the second part of this algorithm we tried to make the source and destination operands of the function's <tt>mov</tt> instructions the same. We also always inserted <tt>add</tt> and <tt>sub</tt> instructions at the start and end of each the function, even if we didn't need to allocate any space on the stack. The source code could also contain functions that have no effect, such as <tt>a = +(a, 0);</tt>. We don't want to write any of these <tt>Instruction</tt>s to the final Assembly output, but instead of deleting them from the <tt>code</tt> <tt>List</tt>s in the <tt>ParsingState</tt> objects we'll add a <tt>isNoOp</tt> method to the <tt>Instruction</tt> class so we can identify them. When we're writing out the code we can just skip over any instruction where <tt>isNoOp</tt> returns true.

~~~~~
@Instruction Members@ += 
boolean isNoOp() { return false; } 
@Other Helpers@ += 
static final Predicate<Instruction> noNoOps = new Predicate<Instruction>() {
  public boolean apply(Instruction i) { return !i.isNoOp(); }
};
~~~~

The implementations in the <tt>Instruction</tt> subclasses are pretty straight-forward; <tt>mov</tt>s and <tt>xchg</tt> instructions are no-ops if both operands are the same, but a <tt>BinaryOp</tt> <tt>Instruction</tt>'s behaviour depends on what its <tt>Op</tt> is.

~~~~
@Move Members@ +=  
public boolean isNoOp() { return from.equals(to); }
@Swap Members@ +=
public boolean isNoOp() { return arg1.equals(arg2); }
@BinaryOp Members@ +=
boolean isNoOp() { return op.isNoOp(arg1, arg2); } 
@Op Members@ +=
public abstract boolean isNoOp(Value arg1, StorableValue arg2);
~~~~

Since the x86 instructions that a <tt>BinaryOp</tt> represents all modify their second operand, only the first operand can be an <tt>Immediate</tt>. If an operand is not an <tt>Immediate</tt> we can't tell at compile-time whether that <tt>Instruction</tt> will have any effect on its second operand, so all of the <tt>isNoOp</tt> checks only return true if the first operand is an <tt>Immediate</tt>. Most of the <tt>Op</tt>s are no-ops if the first operand is zero, for example <tt>a + 0 == a</tt> and <tt>a</tt> shifted left or right by zero bits is still <tt>a</tt>. 

~~~~
@Zero Is Identity@ +=
public boolean isNoOp(Value arg1, StorableValue arg2) { return arg1.equals(new Immediate(0)); }
@ADDQ@ += @Zero Is Identity@
@SUBQ@ += @Zero Is Identity@
@ORQ@ += @Zero Is Identity@
@SHRQ@ += @Zero Is Identity@
@SHLQ@ += @Zero Is Identity@
~~~~

The last two <tt>Op</tt>s are more interesting. Mathematically speaking, any 64-bit value <tt>and</tt>ed with <tt>0xFFFFFFFFFFFFFFFFL</tt> (i.e. 64 1s) will not change, however in x86-64 Assembly code [only](???) <tt>mov</tt> instructions can have 64-bit immediate operands. <tt>and</tt> instructions can have at most 32-bit operands, so although this compiler will output a <tt>andq 0xFFFFFFFFFFFFFFFF, a</tt> instruction when compiling <tt>a = &(a, 0xFFFFFFFFFFFFFFFFL)</tt> the assembler will only output an <tt>andq 0xFFFFFFFF, a</tt> instruction into the binary it generates (and should generate a warning or an error).

Similiarly, we can't decide at this point if a <tt>cmp</tt> instruction is a no-op at compile-time as the second operand has to be a <tt>StorableValue</tt>, so can't be an <tt>Immediate</tt>. While statements in the source code like <tt>a = ?(0x1, 0x1);</tt> can clearly be compiled as <tt>a = 1;</tt> we'll consider this an optimisation performed by the compiler (it's a form of constant propagation), and we've declared above that this compiler won't do any optimisation of the source code.

~~~~
@No Identity@ +=
public boolean isNoOp(Value arg1, StorableValue arg2) { return false; }
@ANDQ@ += @No Identity@
@CMPQ@ += @No Identity@
~~~~

### Writing The Segments ####

We'll only write out a section if it has at least one <tt>Instruction</tt>, so for example if we don't have any zero-initialised <tt>Label</tt>s we won't write out a <tt>.bss</tt> segment. We'll also align each segment as [on some processors](???) that will make accessing the first memory address (either executable code in the <tt>.text</tt> segment or data in the <tt>.bss</tt> or <tt>.data</tt> segments). As all <tt>Value</tt>s in the the compiled language are 64-bits every value stored in the <tt>.data</tt> or <tt>.bss</tt> segments will be 8- or 16-byte aligned. The <tt>.p2align</tt> directive (which is slightly [more portable](???) than the <tt>.align</tt> directive as it as consistent behaviour across all architectures - not that it matters as we're only compiling for x86-64) will make the assembler [insert dummy instructions](http://stackoverflow.com/a/11277804/42543) so that the first <tt>Instruction</tt> of the segment begins on a 16-byte boundary, as the <tt>4</tt> means "ensure the right-most four bits are zero".

~~~~
@Segment x86 Code@ +=
return String.format(".%s\n\t.p2align 4\n", segment);
~~~~

We'll print out the generated assembly code to an <tt>Appendable</tt>. We'll iterate through the linked lists of <tt>Instruction</tt>s starting at the <tt>head</tt> <tt>Instruction</tt> of every function (and the data & bss segments), and use the <tt>Instruction</tt>'s <tt>toString</tt> method to generate the relevant code. We can skip the no-op <tt>Instruction</tt>s we identified earlier, and do some minor formatting of the generated Assembly code: it'll indent everything with a tab unless it's a <tt>Label<tt> <tt>Definition</tt> or a <tt>Segment</tt>.

We'll use this method to write out all three segments. The <tt>.text</tt> segment is the only segment we're writing to which has execute permissions by default, so we'll put all the Assembly code we've generated for the functions there. The <tt>ParsingState</tt>s in the <tt>ProgramState</tt> are added to the <tt>.text</tt> segment without re-ordering, so the functions will appear in the same order that they appear in the source code (with nested functions appearing before the functions they are declared in). It may be more efficient to put the code for functions that call each other next to each other, so when the code for the caller is pulled into the instruction cache any code for the called function on the same cache line will be pulled in for free, saving a memory load when we make the call. We'll also only print the text & bss segments if they have at least one <tt>Definition</tt> (i.e. the <tt>next</tt> pointer of the <tt>Segment</tt> isn't empty).

~~~~
@Append Instructions@ +=
List<Instruction> heads = Lists.newArrayList(
  bss,
  data,
  new Segment("text"));
for(ParsingState function : program.text)
  heads.add(function.head);
for(Instruction start : head)
  for(Instruction i = start; i != null; i = i.next)
    if(!i.isNoOp()) {
      if(!(i instanceof Definition) && !(i instanceof Segment))
        out.append('\t');
      out.append(i.toString()).append('\n');  
    }
~~~~

## Appendices ##

The appendices contain a few pieces of miscellaneous boilerplate. We'll start with the compiler's imports; it uses Google's [Guava](https://code.google.com/p/guava-libraries) libraries as helpers, and the [SWIG Java](http://www.swig.org/Doc1.3/Java.html) bindings to the [GNU GLPK](http://glpk-java.sourceforge.net). It also imports the Antlr runtime library, as the lexer and parser Antlr generates return ASTs of Antlr objects.

~~~~
@Imports@ += 
import static com.blogspot.remisthoughts.compiletoasm.UnsignedLexer.*;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.Register.*;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.WriteType.*;
import static com.google.common.collect.Iterables.*;
import static org.gnu.glpk.GLPK.*;
import java.io.*;
import java.util.*;
import java.util.Map.*;
import java.util.concurrent.atomic.*;
import org.antlr.runtime.ANTLRInputStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.tree.*;
import org.apache.lucene.util.OpenBitSet;
import com.google.common.base.*;
import com.google.common.io.*;
import com.google.common.collect.*;
import org.gnu.glpk.*;
~~~~

### Antlr ###

The Antlr grammar is stored in a separate <tt>.g</tt> file. The grammar has unicode symbols in for the various intrinsic functions, which [antlr supports](http://stackoverflow.com/questions/2081862/how-do-i-match-unicode-characters-in-antlr), but we're using version 3.3, which [doesn't support charVocabulary](http://antlr.1301665.n2.nabble.com/UTF-8-charVocabulary-in-options-in-3-3-td7578297.html), so the grammar specifies unicode characters using syntax like <tt>'\u1234'</tt>.

~~~~
@com/blogspot/remisthoughts/compiletoasm/Unsigned.g:*@ +=
grammar Unsigned;
@Antlr Options@
@Make Antlr Throw On Failure@
@Antlr Tokens@
@Antlr Parse Rules@
~~~~

We need a few lines of code to build an AST from an <tt>InputStream</tt> using the lexer & parser Antlr generated. 

~~~~
@Antlr Options@ +=
options {
  output=AST;
  ASTLabelType=CommonTree;
}
tokens { @Synthetic Tokens@ }
@lexer::header { package com.blogspot.remisthoughts.compiletoasm; }
@header { package com.blogspot.remisthoughts.compiletoasm; }
@Build The AST@ +=
CommonTree ast = new UnsignedParser(
  new CommonTokenStream(
    new UnsignedLexer(
      new ANTLRInputStream(srcIn, Charsets.UTF_8.name())))).eval().tree;
~~~~

The compiler doesn't have useful error messages or provide any diagnostics when a source file is invalid, but we'll at least configure antlr to [throw exceptions on parsing failure](stackoverflow.com/questions/2445008/how-to-get-antlr-3-2-to-exit-upon-first-error). The default behaviour builds an incomplete AST, missing the nodes and tokens that failed to parse.

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

### Other Helpers ###

Some array helpers (we do a lot of linear searching, but not enough to warrant sorting for a binary search or building a hash map).

~~~~
@Other Helpers@ +=
static <T> int indexOf(T[] ts, T t) {
  return Arrays.asList(ts).indexOf(t);
}
~~~~

### The Main Function ###

We'll include a trivial main function so you can run the compiler from the command line. It takes a list of source files and writes the compiled Assembly code to the same folder and same file name (except with the file extension <tt>.s</tt>). The compiler will stop processing the source files on the first failure and will always process a source file (and always overwrite the Assembly file), even if it hasn't changed. We'll always write out the Assembly we generate encoded as UTF-8, even it only ever has ASCII characters in it.

~~~~
@Other Helpers@ +=
public static void main(String[] args) throws Exception {
  for (String inFile : args) {
    File in = new File(inFile);
    String outFile = Files.getNameWithoutExtension(in.getName()) + ".s";
    File out = new File(in.getParentFile(), outFile);
    InputStream inStream = null;
    PrintStream outStream = null;
    try {
      outStream = new PrintStream(out.getAbsolutePath(), Charsets.UTF_8.name());
      outStream.append(".file 1 \"").append(in.getPath()).append("\"\n");
      Compiler.compile(inStream = new FileInputStream(in), outStream);
    } finally {
      Closeables.close(inStream, true);
      Closeables.close(outStream, true);
    }
  }
}
~~~~

### Source Code ###

Finally, all the source code [is available on Github](https://github.com/remis-thoughts/blog/tree/master/compile-to-asm).
