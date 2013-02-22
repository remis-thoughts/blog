# Writing a Compiler: x86 Assembly Code Generation #

http://meri-stuff.blogspot.co.uk/2011/09/antlr-tutorial-expression-language.html#RewriteRules
http://www.antlr.org/wiki/display/~admin/2008/11/30/Example+tree+rewriting+with+patterns

~~~~
@Antlr Parse Rules@ +=
eval : ((statement SC) | fundef)+ -> ^(Root fundef* ^(Definition ^(Name Global["main"]) ^(Parameters) ^(Body statement*)));
statement : funcall | ret | assign | whileloop | ifelse;
expression : variable | Number | funcall | FFI;
variable : Local | Global;
callable : variable| Intrinsic | Conditional | Deref | StackAlloc | FFI;
assignable : variable | Deref assignable -> ^(Deref assignable) | FFI;
ret : Return expression -> ^(Return expression);
assign : assignable Assign expression -> ^(Assign assignable expression);
funcall : callable LP (expression (C expression)*)? RP -> ^(Call callable ^(Parameters expression*));
parameters : (Local (C Local)*)? -> ^(Parameters Local+);
iftrue : (statement SC)*; iffalse : (statement SC)*;
ifelse : If expression LB iftrue RB (Else LB iffalse RB)? 
  -> ^(If ^(Test expression) ^(IfTrue iftrue) ^(IfFalse iffalse)?);
fundef : Definition variable? LP parameters RP LB (statement SC)* RB 
  -> ^(Definition ^(Name variable) parameters ^(Body statement*));
whileloop : While expression LB (statement SC)* RB
  -> ^(While ^(Test expression) ^(Body statement*));
~~~~

_Globals_ are going to end up in the exported symbols of the compiled binary, so have a different set of restrictions on what characters they can contain. _Locals_ are source-level and private symbols so they are essentially arbitrary (but I'm going to enforce a ruby-style variable naming convention). 

~~~~
@Antlr Tokens@ +=
LP : '('; RP : ')'; C : ','; SC : ';'; LB : '{'; RB : '}'; 
Definition : 'fn'; Return : 'return'; Assign : '=';
If : 'if'; Else : 'else'; While : 'while';
Local : '_'* ('a'..'z') ('a'..'z'|'_'|'0'..'9')*;
Global : '_'* ('A'..'Z') ('A'..'Z'|'_'|'0'..'9')*;
FFI : '#' ('a'..'z'|'A'..'Z'|'0'..'9'|'_')+;
Number : ('0'..'9')+;
Intrinsic : '+' | '-' | '&' | '|'; Deref : '@'; StackAlloc : '$';
Conditional : '<' | '>' | '\u2261' | '\u2260' | '\u2264' | '\u2265';
WS : (' ' | '\t' | '\r' | '\n') {$channel=HIDDEN;};
@Synthetic Tokens@ +=
tokens { Parameters; Call; Body; Root; Name; Test; IfTrue; IfFalse; } // for AST re-writing
~~~~

Overall structure of the Java code

~~~~
@com/blogspot/remisthoughts/compiletoasm/Compiler.java:*@ +=
package com.blogspot.remisthoughts.compiletoasm;
@Imports@
public final class Compiler {
  public static void compile(InputStream srcIn, OutputStream asmOut) throws Exception {
    @Build The AST@ 
    ProgramState program = new ProgramState();
    @First Pass@
    @Write The Output@
  }
  @Static Classes@
  @Instruction Classes@
  @Antlr Helpers@
  @Other Helpers@
}
~~~~

What we're compiling for

~~~~
@Other Helpers@ +=
private static UnsignedLong SIZE = UnsignedLong.asUnsigned(8);
~~~~

## (I) AST To Assembly Instructions ## 

[Useful lecture notes](http://www.classes.cs.uchicago.edu/archive/2011/spring/22620-1/docs/handout-03.pdf)
The first pass: turning an AST into pseudo-assembly, with variables, not registers. Each function should be written into its own _List_ of _Instructions_, and we'll have an extra _List_ for _Instructions_ to go at the end of the _text_ section. We'll flatten nested functions - creating randomly-generated _Labels_ for anonymous functions as needed.

~~~~
@First Pass@ +=
program.text.add(Arrays.asList(Compiler.align));
for(Tree child : children(ast)) {
  parseDefinition(child, program);
}
~~~~

Objects to represent the subset of assembly we'll generate. Note that these may correspond to more than one x86 assembly instruction (but only a contiguous series of instructions).

~~~~
@Instruction Classes@ +=
abstract static class Instruction { @Instruction Members@ }
static final class Definition extends Instruction {
  final Label label; final boolean isGlobal; 
  Definition(Label label, boolean isGlobal) { this.label = label; this.isGlobal = isGlobal; }
  public String toString() { @Definition x86 Code@ }
}
enum Op { 
  addq("+") { @ADDQ@ }, 
  subq("-") { @SUBQ@ },
  andq("&") { @ANDQ@ },
  orq("|")  { @ORQ@  },
  cmpq("?") { @CMPQ@ };
  Instruction with(Value arg1, StorableValue arg2) { 
    return new BinaryOp(this, arg1, arg2); 
  }
  @Op Members@
};
static final class BinaryOp extends Instruction {
  final Value arg1; final StorableValue arg2; final Op op;
  BinaryOp(Op op, Value arg1, StorableValue arg2) { this.op = op; this.arg1 = arg1; this.arg2 = arg2; }
  public String toString() { @BinaryOp x86 Code@ }
  @BinaryOp Members@
}
~~~~

Maybe a note on (code) alignment?

~~~~
@Instruction Classes@ +=
private static final Instruction align = new Instruction() {
  public String toString() { return @Align x86 Code@; }
};
@Other Helpers@ +=
private static final UnsignedLong ALIGN_STACK_TO = UnsignedLong.asUnsigned(16);
@Immediate Members@ += 
Immediate alignAllocation() {
  UnsignedLong overhangBytes = value.remainder(ALIGN_STACK_TO);
  if(overhangBytes.equals(ZERO))
    return this;
  else
    return new Immediate(value.add(ALIGN_STACK_TO.subtract(overhangBytes)));
}
~~~~

The data section. Labels are references to symbols defined either in this file or in one of the files linked to.

~~~~
@Static Classes@ +=
static final class Label implements MemoryValue, Comparable<Label> {
  final String name;
  Label(String name) { this.name = name; }
  Label() { this("L" + uniqueness.getAndIncrement()); } // for anonymous fns
  public int compareTo(Label other) {
    return name.compareTo(other.name);
  }
  public String toString() { @Label x86 Code@ }
  @Label Members@
}
~~~~

Some instructions are conditional. Overload this to represent condition intrinsics.

~~~~
@Other Helpers@ +=
enum Condition {
  b("<"), a(">"), e("≡"), ne("≠"), be("≤"), ae("≥");
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
  @Condition Members@
}
~~~~

Moves can be conditional: [and apparently faster](https://mail.mozilla.org/pipermail/tamarin-devel/2008-April/000454.html)

~~~~
@Instruction Classes@ +=
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

Generate x86 GAS code - you could probably use a factory to create the _Instruction_ objects if you wanted to support more than one architecture. See [this](http://stackoverflow.com/a/2752639/42543) for why _.type_ isn't supported on mac. See [lib-c free](https://blogs.oracle.com/ksplice/entry/hello_from_a_libc_free). https://developer.apple.com/library/mac/#documentation/DeveloperTools/Reference/Assembler/000-Introduction/introduction.html . [Instruction set (pdf)](http://download.intel.com/design/intarch/manuals/24319101.pdf). 

~~~~
@Static Classes@ +=
interface Value {}
interface StorableValue extends Value {}
interface MemoryValue extends StorableValue {}
@Value Classes@
static final class Immediate implements Value {
  final UnsignedLong value;
  Immediate(String value) { this.value = UnsignedLong.valueOf(value); }
  Immediate(long value) { this.value = UnsignedLong.asUnsigned(value); }
  private Immediate(UnsignedLong value) { this.value = value; }
  public boolean equals(Object o) { return o instanceof Immediate && ((Immediate)o).value.equals(value);}
  public String toString() { @Immediate x86 Code@ }
  @Immediate Members@
}
static final class Variable implements StorableValue, Resolvable, Comparable<Variable> {
  final String name;
  Variable(String name) { this.name = name; }
  Variable() { this("VAR" + uniqueness.getAndIncrement()); };
  public String toString() { return name; }
  @Variable Members@
}
private static final AtomicInteger uniqueness = new AtomicInteger();
~~~~

The types of values

|| Type || Can Be A Memory Address ||
| Immediate | No (absolute addressing not supported on OSX x86-64) |
| Register | No (it's an absolute address) |
| Label | Always a memory address |

The registers (and their types). NB: omit frame pointer here - and add _THESTACK_ as a synthetic var.

~~~~
@Value Classes@ +=
enum Register implements StorableValue {
  rax, rbx, rcx, rdx, rsp /*stack pointer*/, 
  rbp /*frame pointer*/, rsi, rdi, r8, r9, 
  r10, r11, r12, r13, r14, r15, THESTACK,
  rip /*instruction pointer*/;
  public String toString() { return String.format("%%%s", name()); }
  static final Register[] ASSIGNABLE = { 
    rax, rbx, rcx, rdx, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15, THESTACK 
  };
  static final Register[] CALLEE_SAVES = { rbx, rbp, r12, r13, r14, r15 };
  static final Register[] PARAMETERS = { rdi, rsi, rdx, rcx, r8, r9 };
}
~~~~

Calling functions [CDECL](http://en.wikipedia.org/wiki/X86_calling_conventions#cdecl).

If we have a stack pointer it means we don't know the size of the stack, so we'll use _pushq_ instructions to put arguments on the stack, and we'll pop the arguments off the stack after the function call using an _addq_. However, if we know the stack size exactly we can reserve space on the stack for the parameters at the start of the function, and it'll be reset for free when we leave the function. If there's more than one function we'll reserve space for the function call with the greatest number of stack arguments, so we'll only ever do a single stack allocation and release in the function. However, we don't know if we have a frame pointer until we've processed the whole function, so we'll insert placeholders as we parse calls, and we'll resolve them later.   

~~~~
@Instruction Classes@ +=
static final class Call extends Instruction {
  final Value name;
  Call(Value name) { this.name = name; }
  public String toString() { @Call x86 Code@ }
}
static final class StoreArg extends Instruction {
  final Value arg; final int position;
  final ParsingState function;
  StoreArg(ParsingState function, Value arg, int position) {
    this.arg = arg; this.function = function; 
    this.position = position;
  }
  public String toString() { @Store Arg x86 Code@ }
  @Store Arg Members@
}
static final class ReleaseArgs extends Instruction {
  final int numArgs; final ParsingState function;
  ReleaseArgs(ParsingState function, int numArgs) {
    this.numArgs = numArgs; this.function = function; 
  }
  public String toString() { @Release Args x86 Code@ }
  @Release Args Members@
}
~~~~

~~~~
@Parsing State Members@ +=
private void call(Tree t) {
  List<Value> values = Lists.newArrayList();
  for (Tree argument : children(t, 1)) {
    values.add(getAsValue(argument));
  }
  int numArgs = values.size(), 
      registerArgs = Math.min(numArgs, Register.PARAMETERS.length),
      storedArgs = numArgs - registerArgs;
  for(int r = storedArgs - 1; r >= 0; --r) {
    Value arg = values.get(registerArgs + r);
    if(arg instanceof MemoryValue) {
      Variable tmp = new Variable(); 
      code.add(move(arg, tmp));
      arg = tmp;
    }
    code.add(new StoreArg(this, arg, r));
  }
  for(int r = 0; r < registerArgs; ++r) // regs second so they don't get flattened by mem switch
    code.add(move(values.get(r), Register.PARAMETERS[r]));
  code.add(callFn(text(t, 0), storedArgs));
  code.add(new ReleaseArgs(this, storedArgs));
}
~~~~

Note that we'll assume the function being called is a local variable if it has the same name as a local variable in this function - otherwise it'll be a label which the linker will resolve for us.

~~~~
@Parsing State Members@ +=
private int mostStackArgs = 0;
Instruction callFn(String text, int numStackArgs) {
  Variable fnName = new Variable(text);
  mostStackArgs = Math.max(mostStackArgs, numStackArgs);
  return new Call(declaredVars.contains(fnName) ? fnName : new Label("_" + text));
}
~~~~

According to [the ABI (pdf)](http://www.x86-64.org/documentation/abi.pdf), values are returned in the _rax_ register.

~~~~
@Parse A Return Statement@ +=
Value ret = getAsValue(get(statement,0));
code.add(move(ret, Register.rax));
addReturn();
~~~~

..and after we've returned:

~~~~
@Keep the Return Value@ +=
Variable var = new Variable();
code.add(move(Register.rax, var));
return var;
~~~~

Assignment (to locals and globals)

~~~~
@Parse An Assign Statement@ +=
Value expr = getAsValue(get(statement, 1));
int numDerefs = 0;
while(type(statement, 0) == Deref) {
  ++numDerefs; statement = get(statement, 0);
}
StorableValue to;
if(type(statement, 0) == Global) {
  Label label = new Label(text(statement, 0));
  to = new AtLabel(label); // must be at least one Deref
  @Shortcut For Global Initialisation@
  if(!program.globals.containsKey(label))
    program.globals.put(label, new Immediate(0));
} else {
  Variable var = new Variable(text(statement, 0));
  to = numDerefs > 0 ? new AtAddress(var, 0) : var;
  @Assigned To A Local Variable@
}
for(; numDerefs > 1; -- numDerefs) {
  throw new IllegalStateException();
}
code.add(move(expr, to));
@Shortcut For Global Initialisation@ +=
if(numDerefs == 1 
&& expr instanceof Immediate 
&& !program.globals.containsKey(label)) {
    program.globals.put(label, (Immediate)expr); continue;
} 
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
Value arg1 = getAsValue(get(t, 1, 0));
Value arg2 = getAsValue(get(t, 1, 1));
Variable ret = new Variable();
code.add(move(arg1, ret));
code.add(Op.parse(text(t, 0)).with(arg2, ret));
return ret;
~~~~

Some build-ins are conditional

~~~~
@Condition Members@ +=
String c; 
Condition(String c) { this.c = c; }
private static Condition parse(String c) { 
  for(Condition cond : values()) if(Objects.equal(cond.c, c)) return cond;
  throw new IllegalArgumentException();  
}
@Parse A Conditional Call@ +=
Condition cond = Condition.parse(text(t, 0));
Value arg1 = getAsValue(get(t, 1, 1));
StorableValue arg2 = to(getAsValue(get(t, 1, 0)), StorableValue.class);
code.add(Op.cmpq.with(arg1, arg2));
Variable ret = new Variable();
code.add(move(new Immediate(1), ret));
Variable zero = new Variable();
code.add(move(new Immediate(0), zero));
code.add(new Move(zero, ret, cond.inverse()));
return ret;
~~~~

~~~~
@Parsing State Members@ += 
private Value getAsValue(Tree t) {
      switch (t.getType()) {
        case Definition:
            return parseDefinition(t, program);
        case Global:
            { @Get A Global@ }
        case Local:
            return resolveVar(text(t));
        case FFI:
            return new Label(text(t).substring(1));
        case Call:
            switch(type(t, 0)) {
              case Local:
              case Global:
              case FFI:
                { call(t); @Keep the Return Value@ }
              case Intrinsic: 
                { @Parse An Intrinsic Call@ }
            case Conditional:
              { @Parse A Conditional Call@ }
            case Deref:
              { @Parse A Dereference@ }
            case StackAlloc:
              { @Parse A Stack Allocation@ }
            }
        case Number :
            return new Immediate(t.getText());
        default :
            throw new IllegalArgumentException();
    }
}
private <VALUE extends Value> VALUE to(Value v, Class<VALUE> as) {
  if (as.isAssignableFrom(v.getClass())) return (VALUE) v;
  Variable var = new Variable();
  code.add(move(v, var));
  return (VALUE) var;
}
~~~~

Functions

What if they're anonymous? Need "_" for C linkage

~~~~
@Get Function Name@ +=
myName = hasChildren(def, 0) ? new Label("_" + text(def, 0, 0)) : new Label();
code.add(new Definition(myName, hasChildren(def, 0) && type(def, 0, 0) == Global));
~~~~

~~~~
@Other Helpers@ +=
static Label parseDefinition(Tree def, ProgramState program) {
  ParsingState state = new ParsingState(def, program);  
  state.parseStatements(children(def, 2));
  if(getLast(state.code) != ret) { state.addReturn(); }
  @Store Frame Pointer@
  @Do Register Allocation@
  @Is This The Main Function@
  return state.myName;
}
~~~~

These will come up a lot

~~~~
@Other Helpers@ +=
static final class ProgramState {
  final List<List<Instruction>> text = Lists.newArrayList();
  final Map<Label, Immediate> globals = Maps.newTreeMap();
}
static final class ParsingState {
  List<Instruction> code = Lists.newArrayList();
  Map<Variable, Parameter> params = Maps.newTreeMap(); 
  ProgramState program;
  Set<Variable> declaredVars = Sets.newTreeSet();
  Label myName;
  ParsingState(Tree def, ProgramState program) {
    (this.program = program).text.add(code);
    @Get Function Name@
    @Entering Function@
    @Parse Function Parameters@
    @Keep Track Of Declared Variables@
  }
  @Parsing State Members@
}
~~~~

Parse a block of statements

~~~~
@Parsing State Members@ +=
private void parseStatements(Iterable<Tree> statements) {
  for (Tree statement : statements) {
    switch (type(statement)) {
      case Call :   call(statement); break;
      case Return : @Parse A Return Statement@ break;
      case Assign : @Parse An Assign Statement@ break;
      case While :  @Parse A While Statement@ break;
      case If :     @Parse An If Statement@ break;
    }
  }
}
~~~~

~~~~
@Get A Global@ +=
Label ret = new Label(text(t));
if(!program.globals.containsKey(ret))
  program.globals.put(ret, new Immediate(0));
return ret;
~~~~

Keeping track of user-specified (not auto-inserted) variables in the code. This is for function calls - how do we tell if the string function "name" is a symbol or a variable for dynamic dispatch? We'll assume symbol unless clearly a var as we don't have imports or other ways to tell what symbols can be dynamically linked

~~~~
@Keep Track Of Declared Parameter@ +=
declaredVars.add(v);
@Assigned To A Local Variable@ +=
declaredVars.add(var);
~~~~

Stack frames. [Enter](http://www.read.seas.harvard.edu/~kohler/class/04f-aos/ref/i386/ENTER.htm)
http://www.agner.org/optimize/calling%5Fconventions.pdf http://blogs.embarcadero.com/eboling/2009/05/20/5607
Note that CALLEE_SAVE_VAR shouldn't be user-enterable or this will confuse things, hence "!"

~~~~
@Entering Function@ +=
for(Register r : Register.CALLEE_SAVES)
  code.add(move(r, new Variable(CALLEE_SAVE_VAR + r)));
@Parsing State Members@ +=
void addReturn() {
  for(Register r : Register.CALLEE_SAVES)
    code.add(move(new Variable(CALLEE_SAVE_VAR + r), r));
  code.add(Compiler.ret);
}
@Other Helpers@ +=
private static final String CALLEE_SAVE_VAR = "save!";
~~~~

_ret_ is the return function - and as it takes no arguments we'll just have one global object to represent it.

~~~~
@Instruction Classes@ +=
static final Instruction ret = new Instruction() {
  @Ret Members@
  public String toString() { return @Ret x86 Code@; }
};
@Other Helpers@ += 
static void addBeforeRets(ParsingState state, Instruction inst) {
  for(int i = 0; i < state.code.size(); ++i)
    if(state.code.get(i) == ret) 
      state.code.add(i++, inst);
}
~~~~

If statements 

~~~~
@Parse An If Statement@ +=
Label endIf = parseIf(
  get(statement, 0), // if
  get(statement, 1), // then
  numChildren(statement) == 3 ? get(statement, 2) : null); // else 
if(endIf != null)
  code.add(new DefineLabel(endIf));
@Parsing State Members@ +=
private Label parseIf(Tree test, Tree ifTrue, Tree ifFalse) {
  StorableValue trueOrFalse = to(getAsValue(get(test, 0)), StorableValue.class);
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

We can save a few instructions if we have already done a conditional move

~~~~
@Do If Comparison Check@ +=
Condition jumpCond;
if(getLast(code) instanceof Move && ((Move)getLast(code)).cond != null) {
  jumpCond = ((Move)getLast(code)).cond;
  code.subList(code.size() - 3, code.size()).clear();
} else {
  jumpCond = Condition.e;
  code.add(Op.cmpq.with(new Immediate(0), trueOrFalse));
}
~~~~

~~~~
@Parse An If With No Else@ +=
code.add(new Jump(end, jumpCond));
parseStatements(children(ifTrue));
~~~~

We may not need a final jump from the if-true block as the last instruction could be a _ret_.

~~~~
@Parse An If With An Else@ +=
Label ifFalseLabel = new Label();
code.add(new Jump(ifFalseLabel, jumpCond));
parseStatements(children(ifTrue));
if(getLast(code).isNextInstructionReachable())
  code.add(new Jump(end, null));
else
  end = null;
code.add(new DefineLabel(ifFalseLabel));
parseStatements(children(ifFalse));
~~~~

Jumping about

~~~~
@Instruction Classes@ +=
private static class DefineLabel extends Instruction {
  final Label label;
  DefineLabel(Label label) { this.label = label; }
  public String toString() { @Define Label x86 Code@ }
  public boolean equals(Object o) { return o instanceof DefineLabel && ((DefineLabel)o).label.equals(label); }
}
private static class Jump extends Instruction {
  final Label to; final Condition cond;
  Jump(Label to, Condition cond) { 
    this.to = to; this.cond = cond; 
  }
  public String toString() { @Jump x86 Code@ }
  @Jump Members@
}
~~~~

A while loop is essentially an if block with an unconditional loop

~~~~
@Parse A While Statement@ +=
Label l = new Label(); code.add(new DefineLabel(l));
Label endWhile = parseIf(get(statement, 1), get(statement, 2), null);
code.add(new Jump(l, null)); 
code.add(new DefineLabel(endWhile)); 
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
if(state.myName.name.equals("_main")) {
  state.code.addAll(1, Arrays.asList(
    new Push(rsp),
    Op.andq.with(new Immediate(0xFFFFFFFFFFFFFFF0L), rsp)));
  addBeforeRets(state, new Pop(rsp));
}
@Instruction Classes@ +=
private static final class Push extends Instruction {
  final Value value;
  Push(Value value) { this.value = value; }
  public String toString() { @Push x86 Code@ }
  @Push Members@
}
private static final class Pop extends Instruction {
  final StorableValue value;
  Pop(StorableValue value) { this.value = value; }
  public String toString() { @Pop x86 Code@ }
}
~~~~

...and when we're calling a function, we should preserve stack alignment, so if we're pushing an odd number of args onto the stack we'll have to pad the stack with 8 bytes first to leave it at a 16 byte boundary. 

~~~~
@Before We Push Function Params Onto Stack@ += 
if(numParams > maxInRegs && (numParams - maxInRegs) % 2 != 0)
  state.code.add(Op.subq.with(new Immediate(8), rsp));
@Remove Function Call Params From Stack@ +=
int correction = 0;
if((numParams - maxInRegs) % 2 != 0) correction = 1;
state.code.add(Op.addq.with(new Immediate((numParams - maxInRegs + correction) * SIZE.longValue()), rsp));
~~~~

Memory stuff. Need _MemoryAddress_ interface as we can't have x86 instructions like _movq (%rax), (%rdi)_

~~~~
@Static Classes@ +=
static final class AtAddress implements MemoryValue, Resolvable {
  final StorableValue at; final long offset;
  AtAddress(StorableValue at, long offset) {
    this.at = at; this.offset = offset;
  }
  public String toString() { @At Address x86 Code@ }
  public boolean equals(Object o) {
    if(!(o instanceof AtAddress)) return false;
    AtAddress other = (AtAddress) o;
    return at.equals(other.at) && offset == other.offset;
  }
  public int hashCode() { return at.hashCode(); }
  @At Address Members@
}
private static final class AtLabel implements MemoryValue {
  final Label at;
  AtLabel(Label at) { this.at = at; }
  public String toString() { @At Label x86 Code@ }
}
@Parse A Dereference@ +=
StorableValue val = to(getAsValue(get(t, 1, 0)), StorableValue.class);
if(val instanceof Label)
  return new AtLabel((Label)val);
else if(val instanceof Variable)
  return new AtAddress(val, 0);
else {
  Variable ret = new Variable();
  code.add(move(val, ret));
  return new AtAddress(ret, 0);
} 
~~~~

The stack. Each time we refer to a stack-allocated variable we have to get the same memory address. Also we're only doing statically-determined sizes  - otherwise we'd have to keep the size around in a variable and free it every time we left a function (even assuming it's allocated on all code paths through the system). A stack-size-per node count would help with the nudging problem too...maybe we need stackalloc and stackfree functions? But how do we free something that's in the middle of the stack? Maybe have a static function and a dynamic allocation function? 

We'll have two types of stack allocation - static (where we know how many bytes we'll put on the stack at compile-time) and dynamic. This'll affect how we refer to function parameters on the stack and parameters to functions we call. These classes'll therefore need to refer to the _ParsingState_ to see whether we're using dynamic allocation.

~~~~
@Value Classes@ +=
private static abstract class UsesStack implements MemoryValue {
  final ParsingState function;
  UsesStack(ParsingState function) { this.function = function; }
}
@Parsing State Members@ +=
private StorableValue framePointer = null; // avoid setting if possible
~~~~

Stack variables are _Values_ that are pointers onto this function's stack space.

~~~~
@Value Classes@ +=
static final class StaticStackVar extends UsesStack {
  final UnsignedLong bytesIntoStack;
  StaticStackVar(ParsingState function, UnsignedLong bytesIntoStack) {
    super(function); this.bytesIntoStack = bytesIntoStack;
  }
  public String toString() { @StackVar x86 Code@ }
}
@Parsing State Members@ +=
private final Map<Variable, StaticStackVar> stackVars = Maps.newTreeMap();
private UnsignedLong stackBytes = ZERO;
StaticStackVar stackVar(Variable name, Immediate bytes) {
  if(!stackVars.containsKey(name)) {
    stackVars.put(name, new StaticStackVar(this, stackBytes));
    stackBytes = stackBytes.add(bytes.value);
  } 
  return stackVars.get(name); 
}
~~~~

Create stack variables by subtracting chunks from the stack, or pre-allocating the space if we know it beforehand.

~~~~
@Parse A Stack Allocation@ +=
Value size = getAsValue(get(t, 1, 0));
if(size instanceof Immediate)
  return stackVar(new Variable(), (Immediate)size);
else {
  if(framePointer == null) 
    framePointer = new Variable();
  Variable ret = new Variable();
  code.add(move(Register.rsp, ret)); // current top of stack is now pointer to allocated memory
  code.add(Op.subq.with(size, Register.rsp));
}
~~~~

Dealing with parameters. Make _Variable_ comparable and add _equals_ methods so we can use it as a key to a collection. Note thatsome are passed in registers and others on the stack. If we have a stack pointer we'll just push these, as we don't know where the top of the stack is (we'll only have a stack pointer if we've dynamically allocated memory). However, under static-only allocation, we can pre-allocate the space needed for the maximum number of parameters (choosing to save CPU cycles over stack bytes).

~~~~
@Static Classes@ +=
static final class Parameter extends UsesStack {
  final int number;
  Parameter(ParsingState function, int number) { 
    super(function); this.number = number; 
  }
  public String toString() { @Parameter x86 Code@ }
}
~~~~

We'll keep a track of offsets below the current frame's pointer (later parameters get bigger offsets) - and we'll either subtract these from the frame pointer or subtract these plus static allocation size from the stack pointer depending on whether we're doing statci+dynamic or static-only allocation.

~~~~
@Parse Function Parameters@ +=
for(int p = 0, count = numChildren(def, 1); p < count; ++p) {
  Variable v = new Variable(text(def, 1, p));
  if(p < Register.PARAMETERS.length)
    code.add(move(Register.PARAMETERS[p], v));
  else
    params.put(v, new Parameter(this, p - Register.PARAMETERS.length));
  @Keep Track Of Declared Parameter@
}
~~~~

When we're parsing a function call, we'll keep track of the maximum number of stack parameters, so if we're statically-only allocating we'll know how much space we'll need.


~~~~
@Parsing State Members@ +=
StorableValue resolveVar(String text) {
  Variable v = new Variable(text);
  return params.containsKey(v) ? params.get(v) : v;
}
@Variable Members@ +=
public boolean equals(Object o) { return o instanceof Variable && ((Variable)o).name.equals(name); }
public int hashCode() { return name.hashCode(); }
public int compareTo(Variable o) { return name.compareTo(o.name); }
~~~~

Now we know whether or not we're using dynamic stack allocation, but we still don't know how many static bytes we'll need as we might spill some variables to the stack in the register allocation phase (next). However, we'll insert the stack pointer code here so it gets allocated correctly. 

~~~~
@Store Frame Pointer@ +=
if(state.framePointer != null) {
  state.code.add(1, move(Register.rsp, state.framePointer));
  addBeforeRets(state, move(state.framePointer, Register.rsp));
}
~~~~


## (II) Register Allocation via Linear Programming ##

We'll do this function-by-function
http://www.xypron.de/projects/linopt/apidocs/index.html (the paper)[http://grothoff.org/christian/lcpc2006.pdf]

~~~~
@Do Register Allocation@ +=
ControlFlowGraph controlFlow = new ControlFlowGraph(state.code);
if(controlFlow.variables.length > 0) {
  @Solve@
}
@Other Helpers@ +=
static glp_prob getLPproblem(ControlFlowGraph controlFlow) {
  glp_prob allocation = glp_create_prob();
  @Determine Rows And Columns@
  @Set Up Objective@
  @Finalise Problem@
  return allocation;
}
static void copySolutionIntoCode(glp_prob allocation, ControlFlowGraph controlFlow, ParsingState state) {
  @Copy Solution Into Code@
  @Insert Stack Moves@
  @Allocate Stack Space For Variables@
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
@Jump Members@ += 
boolean isNextInstructionReachable() { return cond != null; }
Label couldJumpTo() { return to; }
~~~~

We need two separate bits of information from this graph; how the nodes relate to each other and how the instructions fit in between the nodes. 

~~~~
@Other Helpers@ +=
static final class ControlFlowGraph {
  @Control Flow Graph Members@
  ControlFlowGraph(List<Instruction> code) {
    @Get Variables Used@
    @Fit Instructions Between Nodes@
    @Map Nodes To Instructions@
    @Join Nodes Together@
    @Variables Used At Next Instruction@
    @Identify Function Calls@
    @Calculate Liveness@
    @Mark Aliased Variables@
    @Generate GLPK Mappings@
  }
}
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
  for(Variable variable : code.get(i).readsFrom())
    varsReadNext.set(prevNode[i] * variables.length + indexOf(variables, variable));
for(Entry<Integer, Collection<Integer>> next : instructionsFollowingNode.asMap().entrySet()) { // not empty
  Set<Variable> varsSet = Sets.newHashSet(variables); 
  for(int i : next.getValue())
    varsSet.retainAll(code.get(i).willWriteTo());
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

Data dependencies - will let us calculate liveness. Can write to is a subset (but not a strict subset) of will write to

~~~~
@Instruction Members@ +=
Set<Variable> readsFrom() { return noVars; }
Set<Variable> willWriteTo() { return noVars; }
<VAL> Set<VAL> uses(Class<VAL> c) { return Collections.emptySet(); }
@BinaryOp Members@ +=
Set<Variable> readsFrom() { return setOf(Variable.class, arg1, arg2); }
Set<Variable> willWriteTo() { 
  return arg2 instanceof Variable ? setOf(Variable.class, arg2) : noVars; 
}
<VAL> Set<VAL> uses(Class<VAL> c) { return setOf(c, arg1, arg2); }
@Move Members@ +=
Set<Variable> readsFrom() { 
  return to instanceof AtAddress ? setOf(Variable.class, from, to) : setOf(Variable.class, from); 
}
Set<Variable> willWriteTo() {
  return to instanceof Variable && cond == null ? setOf(Variable.class, to) : noVars; 
}
<VAL> Set<VAL> uses(Class<VAL> c) { return setOf(c, from, to); }
@Store Arg Members@ +=
Set<Variable> readsFrom() { return setOf(Variable.class, arg); }
<VAL> Set<VAL> uses(Class<VAL> c) { return setOf(c, arg); }
@Other Helpers@ +=
private static <VAL> Set<VAL> setOf(Class<VAL> c, Object... objs) {
  return ImmutableSet.copyOf(filter(transform(Arrays.asList(objs), unwrap), c));
}
private static Function<Object, Object> unwrap = new Function<Object, Object>() {
  public Object apply(Object v) { 
    return v instanceof AtAddress ? ((AtAddress)v).at : v;
  }
};
private static final Set<Variable> noVars = Collections.emptySet();
~~~~

We need to know all the variables we'll ever need to assign

~~~~
@Get Variables Used@ +=
Set<Variable> allVariables = Sets.newTreeSet();
for(Instruction i : code) {
  addAll(allVariables, i.uses(Variable.class));
}
variables = toArray(allVariables, Variable.class);
~~~~

Liveness...as a BitSet. A variable is not live at the node before the instruction where it's created and is not live at the node after the instruction where it's last read.

~~~~
@Control Flow Graph Members@ +=
final BitSet isLive;
boolean isLiveAt(int variable, int node) {
  return isLive.get(node * variables.length + variable);
}
@Calculate Liveness@ +=
isLive = (BitSet) varsReadNext.clone(); 
Queue<Integer> unprocessed = Queues.newArrayDeque(Arrays.asList(ControlFlowGraph.LAST_NODE));
BitSet nodesSeen = new BitSet();
for(Integer node = unprocessed.poll(); node != null; node = unprocessed.poll()) {
  boolean changed = false;
  for(int n : this.nextNodes.get(node))
      changed |= copyFrom(isLive, n, node, variables.length);
  for(int index = node * variables.length; index < (node+1) * variables.length; ++index)
    if(varsWrittenNext.get(index) && !varsReadNext.get(index))
      isLive.clear(index);
  if(!nodesSeen.get(node) || changed)
    for(int n : this.nextNodes.inverse().get(node)) unprocessed.offer(n);
  nodesSeen.set(node);
}
~~~~

We can use liveness information to decide what variables we need to assign to registers at each node. At function calls we should ensure that any live variables are stored in registers that'll be preserved. The assembly instructions inserted before the function call itself will ensure that the parameters will stay in the correct registers. Also, if a variable is going to be used in an instruction following a node that variable can't be assigned to the stack!

~~~~
@Control Flow Graph Members@ +=
final Variable[] variables;
boolean needsAssigning(int variable, int node, Register r) {
  boolean isLive = isLiveAt(variable, node) || isVarUsedNext(variable, node);
  boolean thisRegPreserved = !nodesBeforeFnCalls.get(node) || calleeSavesRegisterIndices.get(r.ordinal());
  boolean ifUsedNotOnStack = !(isVarUsedNext(node, variable) && r == Register.THESTACK);
  return isLive && thisRegPreserved && ifUsedNotOnStack;
}
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
@Other Helpers@ +=
static abstract class CellKey implements Comparable<CellKey> {
  final int v; final Register r; final int n;
  CellKey(int v, Register r, int n) { 
    this.v = v; this.n = n; this.r = r;
  }
  public int compareTo(CellKey o) {
    return ComparisonChain.start().
      compare(getClass().getName(), o.getClass().getName()).
      compare(v, o.v).
      compare(r, o.r).
      compare(n, o.n).result();
  }
  public String toString() {
    return String.format("%s v:%d r:%s n:%d", getClass().getSimpleName(), v, r, n);
  }
}
private static final int NONE = -1;
~~~~

First: columns. They make up part of the objective function

~~~~
@Other Helpers@ +=
static abstract class ColumnKey extends CellKey {
  ColumnKey(int v, Register r, int n) { super(v, r, n); }
  abstract void addToObjective(glp_prob allocation, int columnIndex, ControlFlowGraph cfg);
}
@Control Flow Graph Members@ +=
final List<ColumnKey> columns = Lists.newArrayList();
@Generate GLPK Mappings@ +=
@Add Column Keys@
Collections.sort(columns);
~~~~

The first (and most important) set of unknowns are the _VarInRegAtInstr_ columns: which register each variable is in at each node in the control flow graph, assuming it's live. 

~~~~~
@Other Helpers@ +=
static final class VarInRegAtInstr extends ColumnKey {
  VarInRegAtInstr(int v, Register r, int n) { super(v, r, n); }
  void addToObjective(glp_prob allocation, int columnIndex, ControlFlowGraph cfg) {
    double coefficient = 0d;
    @Determine VarInRegAtInstr Coefficient@
    glp_set_obj_coef(allocation, columnIndex + 1, coefficient);
  }
}
@Add Column Keys@ +=
for (int v = 0; v < variables.length; ++v)
  for(int n = 0; n < numNodes; ++n)
    for (Register r : Register.ASSIGNABLE)
      if(needsAssigning(v, n, r))
        columns.add(new VarInRegAtInstr(v, r, n));
~~~~

The next set of unknowns are used to work out the cost of a given assignment. We'll use constraints to ensure that the _NEW_VAR_IN_REG_FLAG_ columns have a one for node _n_, register _r_ and variable _v_ if register _r_ had a different variable in before node _n_ and now has variable _v_ in, and in all other circumstances _NEW_VAR_IN_REG_FLAG_ is zero. This makes the variable an indicator of whether we've loaded a variable into a register from the stack, so it'll be an important part of the objective function (as we want to minimise stack usage).

The cost of register access, _mu_ in (the paper)[http://grothoff.org/christian/lcpc2006.pdf], can be a function of the register and the instruction. This gives us the option of discouraging moves in frequently-used instructions (such as those in the body of a loop). Note that we don't check for liveness here as we don't even think about preserving a non-live variable.

~~~~    
@Other Helpers@ +=
static final class NewVarInRegFlag extends ColumnKey {
  NewVarInRegFlag(int v, Register r, int n) { super(v, r, n); }
  void addToObjective(glp_prob allocation, int columnIndex, ControlFlowGraph cfg) {
    glp_set_obj_coef(allocation, columnIndex + 1, COST_OF_REG_ACCESS);
  }
}
private static final double COST_OF_REG_ACCESS = 1.0;
@Add Column Keys@ +=
for (int v = 0; v < variables.length; ++v)
  for(int n = 0; n < numNodes; ++n)
    for (Register r : Register.ASSIGNABLE)
      if(needsAssigning(v, n, r))
        columns.add(new NewVarInRegFlag( v, r, n));
~~~~


Let's tell GLPK how many columns we'll need in total

~~~~
@Determine Rows And Columns@ +=
glp_add_cols(allocation, controlFlow.columns.size());
glp_add_rows(allocation, controlFlow.rows.size());
for(int c = 0; c < controlFlow.columns.size(); ++c)
  glp_set_col_kind(allocation, c + 1, GLP_BV);
~~~~

~~~~
@Other Helpers@ +=
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
@Determine VarInRegAtInstr Coefficient@ +=
if(r == Register.THESTACK) coefficient += spillCost(v, n, cfg);
~~~~

The objective: minimise spillage at each node. 

~~~~
@Set Up Objective@ +=
glp_set_obj_name(allocation, "spillage");
glp_set_obj_dir(allocation, GLP_MIN);
for(int c = 0; c < controlFlow.columns.size(); ++c)
  controlFlow.columns.get(c).addToObjective(allocation, c, controlFlow);
~~~~

~~~~
@Control Flow Graph Members@ +=
final Multimap<Integer, Integer> aliasingVars = TreeMultimap.create();
@Mark Aliased Variables@ +=
for (int n = 0; n < numNodes; ++n) {
  Collection<Integer> instrsAfter = instructionsFollowingNode.get(n);
  if(instrsAfter.size() != 1) continue;
  int i = getOnlyElement(instrsAfter); Instruction instr = code.get(i); 
  if(!(instr instanceof Move)) continue;
  Move move = (Move) instr;
  if(move.uses(Variable.class).size() != 2) continue; 
  if(move.cond != null) continue;
  if(instr.readsFrom().size() == 2) continue; // reg->mem
  int reads = indexOf(variables, getOnlyElement(instr.readsFrom()));
  int writes = indexOf(variables, getOnlyElement(instr.willWriteTo()));
  if(isLiveAt(reads, nextNode[i]) || isLiveAt(writes, n)) continue;
  aliasingVars.putAll(n, Arrays.asList(reads, writes));
}
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
@Control Flow Graph Members@ +=
private final Set<ColumnKey> shouldBeNoOp = Sets.newTreeSet();
List<Integer> nodesAround(int instruction) {
  return Arrays.asList(prevNode[instruction], nextNode[instruction]);
}
@Generate GLPK Mappings@ +=
for(int i = 0; i < code.size(); ++i) {
  if(!(code.get(i) instanceof Move)) continue;
  for(Variable variable : code.get(i).uses(Variable.class)) {
    int v = indexOf(variables, variable);
    for(Register r : code.get(i).uses(Register.class))
      for(int n : nodesAround(i)) 
        if(needsAssigning(v, n, r))
          shouldBeNoOp.add(new VarInRegAtInstr(v, r, n));
  }
}
@Determine VarInRegAtInstr Coefficient@ +=
if(cfg.shouldBeNoOp.contains(this)) coefficient += MOVE_NOP_HINT;
@Other Helpers@ +=
private static final double MOVE_NOP_HINT = -0.5;
~~~~

Let's also encourage putting aliased vars in the same register. For that we'll need another flag:

~~~~
@Other Helpers@ +=
static final class AliasedInSameReg extends ColumnKey {
  AliasedInSameReg(Register r, int n) { super(NONE, r, n); }
  void addToObjective(glp_prob allocation, int columnIndex, ControlFlowGraph cfg) {
    glp_set_obj_coef(allocation, columnIndex + 1, ALIASED_VAR_HINT);
  }
}
private static final double ALIASED_VAR_HINT = -0.5;
@Add Column Keys@ +=
for(Integer n : aliasingVars.asMap().keySet())
  for (Register r : Register.ASSIGNABLE)
    if(r != Register.THESTACK)
      columns.add(new AliasedInSameReg(r, n));
~~~~

Now, onto the rows. Similar to column keys...

~~~~
@Other Helpers@ +=
static abstract class RowKey extends CellKey {
  RowKey(int v, Register r, int n) { super(v, r, n); }
  abstract void addConstraints(
    glp_prob allocation,
    int row,
    List<Constraint> constraints,
    ControlFlowGraph cfg);
}
@Control Flow Graph Members@ +=
final List<RowKey> rows = Lists.newArrayList();
@Generate GLPK Mappings@ +=
@Add Row Keys@
Collections.sort(rows);
~~~~

Constraints:

~~~~
@Other Helpers@ +=
static final class Constraint {
  final int row; final int column; final double value;
  Constraint(int row, int column, double value) {
    this.row = row; this.column = column; this.value = value;
  } 
}
@Determine Rows And Columns@ +=
List<Constraint> constraints = Lists.newArrayList();
for(int row = 0; row < controlFlow.rows.size(); ++row)
  controlFlow.rows.get(row).addConstraints(allocation, row, constraints, controlFlow);
@Finalise Problem@ +=
SWIGTYPE_p_int constraintCols = new_intArray(constraints.size());
SWIGTYPE_p_int constraintRows = new_intArray(constraints.size());
SWIGTYPE_p_double constraintVals = new_doubleArray(constraints.size());
for(int i = 0; i < constraints.size(); ++i) {
  Constraint c = constraints.get(i);
  intArray_setitem(constraintCols, i + 1, c.column + 1);
  intArray_setitem(constraintRows, i + 1, c.row + 1);
  doubleArray_setitem(constraintVals, i + 1, c.value);
}
glp_load_matrix(
  allocation,
  constraints.size(),
  constraintRows,
  constraintCols,
  constraintVals);
~~~~

~~~~
@Other Helpers@ +=
static final class AliasedVarFlag extends RowKey {
  AliasedVarFlag(Register r, int n) { super(NONE, r, n); }
  void addConstraints(glp_prob allocation, int row, List<Constraint> constraints, ControlFlowGraph cfg) {
    Collection<Integer> aliasing = cfg.aliasingVars.get(n);
    int constraintType = aliasing.size() == 1 ? GLP_FX : GLP_DB;
    glp_set_row_bnds(allocation, row + 1, constraintType, 0.0, aliasing.size() - 1.0);
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new AliasedInSameReg(r, n)),
      -1.0 * aliasing.size()));
    for(int v : cfg.aliasingVars.get(n))
      constraints.add(new Constraint(
        row, 
        Collections.binarySearch(cfg.columns, new VarInRegAtInstr(v, r, n)),
        1.0));
  }
}
@Add Row Keys@ +=
for(Integer n : aliasingVars.asMap().keySet())
  for (Register r : Register.ASSIGNABLE)
    if(r != Register.THESTACK)
      rows.add(new AliasedVarFlag(r, n));
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
@Other Helpers@ +=
static final class OneVarPerReg extends RowKey {
  final Set<Integer> vars = Sets.newTreeSet();
  OneVarPerReg(int avoid, Register r, int n) { super(avoid, r, n); }
  void addConstraints(glp_prob allocation, int row, List<Constraint> constraints, ControlFlowGraph cfg) {
    glp_set_row_bnds(allocation, row + 1, GLP_DB, 0.0, 1.0);
    for(int v : vars)
      constraints.add(new Constraint(
        row, 
        Collections.binarySearch(cfg.columns, new VarInRegAtInstr(v, r, n)),
        1.0));
  }
}
@Add Row Keys@ +=
for (int n = 0; n < numNodes; ++n)
  for (Register r : Register.ASSIGNABLE)
    if(r != Register.THESTACK)
      if(aliasingVars.containsKey(n)) {
        for(int avoid : aliasingVars.get(n)) {
          OneVarPerReg constraint = new OneVarPerReg(avoid, r, n);
          for (int v = 0; v < variables.length; ++v)
            if(v != avoid && needsAssigning(v, n, r))
              constraint.vars.add(v);
        }
      } else {
        OneVarPerReg constraint = new OneVarPerReg(NONE, r, n);
        for (int v = 0; v < variables.length; ++v)
          if(needsAssigning(v, n, r))
            constraint.vars.add(v);
        rows.add(constraint);
      }
~~~~

Constraint: variables must not be lost; they must be in either the stack or exactly one register. While technically we could put the variable on stack as well as in more than one register, this makes the LP solver much slower as there are so many more possible combinations of assignments to narrow down. Also, if a variable's not live then force it on the stack.

~~~~
@Other Helpers@ +=
static final class VarsNotLost extends RowKey {
  Set<Register> registers = EnumSet.noneOf(Register.class);
  VarsNotLost(int v, int n) { super(v, Register.THESTACK, n); }
  void addConstraints(glp_prob allocation, int row, List<Constraint> constraints, ControlFlowGraph cfg) {
    glp_set_row_bnds(allocation, row + 1, GLP_FX, 1.0, 1.0);
    for(Register r : registers)
      constraints.add(new Constraint(
        row, 
        Collections.binarySearch(cfg.columns, new VarInRegAtInstr(v, r, n)),
        1.0));
  }
}
@Add Row Keys@ +=
for (int v = 0; v < variables.length; ++v)
  for (int n = 0; n < numNodes; ++n) {
    VarsNotLost constraint = new VarsNotLost(v, n);
    for (Register r : Register.ASSIGNABLE)
      if(needsAssigning(v, n, r))
        constraint.registers.add(r);
    if(!constraint.registers.isEmpty())
      rows.add(constraint);
  }
~~~~

The last pair of constraints ensure that the _NEW_VAR_IN_REG_FLAG_ works as we want. Note that we set the constraints in pairs of nodes, at least one of which _needsAssigning_. We have to overhang (i.e. we don't only add constraints if both nodes _needsAssigning_) as if we don't we'll miss some constraints on the border, and so the flag will be undefined. A NEW_VAR_IN_REG_FLAG value of 1 means the variable just arrived in this register from somewhere. 

~~~~
@Add Row Keys@ +=
for (int v = 0; v < variables.length; ++v)
  for (Register r : Register.ASSIGNABLE)
    for(Entry<Integer, Integer> n : this.nextNodes.entries()) {
      if(needsAssigning(v, n.getKey(), r)
      && needsAssigning(v, n.getValue(), r)) 
        { @Add Constraints If Both Nodes Are Relevant@ }
      else if(needsAssigning(v, n.getValue(), r))
        { @Force When Variable First Live@ }
      else if(needsAssigning(v, n.getKey(), r))
        { @Force Zero When Variable Dies@ }
    }
~~~~

~~~~
@Other Helpers@ +=
static final class ForceZeroWhenVariableDies extends RowKey {
  ForceZeroWhenVariableDies(int v, Register r, int n) { super(v, r, n); }
  void addConstraints(glp_prob allocation, int row, List<Constraint> constraints, ControlFlowGraph cfg) {
    glp_set_row_bnds(allocation, row + 1, GLP_FX, 0.0, 0.0);
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new NewVarInRegFlag(v, r, n)),
      1.0));
  }
}
@Force Zero When Variable Dies@ +=
rows.add(new ForceZeroWhenVariableDies(v, r, n.getKey()));
~~~~

~~~~
@Other Helpers@ +=
static final class ForceWhenVariableFirstLive extends RowKey {
  final boolean setToOne;
  ForceWhenVariableFirstLive(int v, Register r, int n, boolean setToOne) { 
    super(v, r, n); this.setToOne = setToOne;
  }
  void addConstraints(glp_prob allocation, int row, List<Constraint> constraints, ControlFlowGraph cfg) {
    glp_set_row_bnds(allocation, row + 1, GLP_FX, 0.0, 0.0);
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new VarInRegAtInstr(v, r, n)),
      setToOne ? -1.0 : 0.0));
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new NewVarInRegFlag(v, r, n)),
      1.0));
  }
}
@Force When Variable First Live@ +=
boolean setToOne = isLiveAt(v, n.getKey()) || r == Register.THESTACK;
rows.add(new ForceWhenVariableFirstLive(v, r, n.getValue(), setToOne));
~~~~

~~~~
@Other Helpers@ +=
static final class ZeroIfInRegisterPreviously extends RowKey {
  final int nTo;
  ZeroIfInRegisterPreviously(int v, Register r, int nFrom, int nTo) { 
    super(v, r, nFrom); this.nTo = nTo;
  }
  /** if R(n-1) = 1 then F(n) = 0 */
  void addConstraints(glp_prob allocation, int row, List<Constraint> constraints, ControlFlowGraph cfg) {
    glp_set_row_bnds(allocation, row + 1, GLP_UP, 0.0, 1.0);
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new VarInRegAtInstr(v, r, n)),
      1.0));
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new NewVarInRegFlag(v, r, nTo)),
      1.0));
  }
  public int compareTo(CellKey o) {
    return ComparisonChain.start().
      compare(super.compareTo(o), 0).
      compare(nTo, o instanceof ZeroIfInRegisterPreviously ? ((ZeroIfInRegisterPreviously)o).nTo : NONE).
      result();
  }
}
@Add Constraints If Both Nodes Are Relevant@ +=
rows.add(new ZeroIfInRegisterPreviously(v, r, n.getKey(), n.getValue()));
~~~~

~~~~
@Other Helpers@ +=
static final class ZeroIfNotInRegister extends RowKey {
  ZeroIfNotInRegister(int v, Register r, int n) { super(v, r, n); }
  /** if R(n) = 0 then F(n) = 0 */
  void addConstraints(glp_prob allocation, int row, List<Constraint> constraints, ControlFlowGraph cfg) {
    glp_set_row_bnds(allocation, row + 1, GLP_LO, 0.0, 0.0);
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new VarInRegAtInstr(v, r, n)),
      1.0));
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new NewVarInRegFlag(v, r, n)),
      -1.0));
  }
}
@Add Constraints If Both Nodes Are Relevant@ +=
rows.add(new ZeroIfNotInRegister(v, r, n.getValue()));
~~~~

~~~~
@Other Helpers@ +=
static final class SetFlagOnChange extends RowKey {
  final int nTo;
  SetFlagOnChange(int v, Register r, int nFrom, int nTo) { 
    super(v, r, nFrom); this.nTo = nTo;
  }
  /** if R(n-1) - R(n) + F(n) >= 0 so when R(n) = 1 and R(n-1) = 0 then F(n) must be 1 */
  void addConstraints(glp_prob allocation, int row, List<Constraint> constraints, ControlFlowGraph cfg) {
    glp_set_row_bnds(allocation, row + 1, GLP_LO, 0.0, 0.0);
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new VarInRegAtInstr(v, r, n)),
      1.0));
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new VarInRegAtInstr(v, r, nTo)),
      -1.0));
    constraints.add(new Constraint(
      row, 
      Collections.binarySearch(cfg.columns, new NewVarInRegFlag(v, r, nTo)),
      1.0));
  }
  public int compareTo(CellKey o) {
    return ComparisonChain.start().
      compare(super.compareTo(o), 0).
      compare(nTo, o instanceof SetFlagOnChange ? ((SetFlagOnChange)o).nTo : NONE).
      result();
  }
}
@Add Constraints If Both Nodes Are Relevant@ +=
rows.add(new SetFlagOnChange(v, r, n.getKey(), n.getValue()));
~~~~

[All GLPK options](http://www.maximal-usa.com/solvopt/optglpk.html) - note presolve defaults to off.
[GLPK manual](http://www.mai.liu.se/~kahol/kurser/all/glpk.pdf)

~~~~
@Solve@ +=
glp_prob allocation = getLPproblem(controlFlow);
glp_iocp iocp = new glp_iocp();
glp_init_iocp(iocp);
iocp.setPresolve(GLP_ON);
if (glp_intopt(allocation, iocp) != 0)
  throw new IllegalStateException();
copySolutionIntoCode(allocation, controlFlow, state);
glp_delete_prob(allocation);
~~~~

We know which register each variable is in at each node now (assuming it's live):

~~~~
@Other Helpers@ +=
static Register whereAmI(glp_prob allocation, ControlFlowGraph controlFlow, int v, int n) {
  for(Register r : Register.ASSIGNABLE) {
    int index = Collections.binarySearch(controlFlow.columns, new VarInRegAtInstr(v, r, n));
    if(index >= 0 && glp_mip_col_val(allocation, index + 1) > 0.5)
        return r;
  }
  throw new IllegalStateException(); // shouldn't get here
}
~~~~

Now we know where to put variables, we can update the instructions. We'll make the instructions immutable, and so we need copy-on-resolve semantics.

~~~~
@Instruction Members@ +=
Instruction resolve(Variable var, Register reg) { return this; }
@BinaryOp Members@ +=
Instruction resolve(Variable var, Register reg) { 
  return op.with(resolve(arg1, var, reg), resolve(arg2, var, reg)); 
}
@Store Arg Members@ += 
Instruction resolve(Variable var, Register reg) {
  return new StoreArg(function, resolve(arg, var, reg), position);
}
@Move Members@ +=
Instruction resolve(Variable var, Register reg) {
  return new Move(resolve(from, var, reg), resolve(to, var, reg), cond); 
}
~~~~

~~~~
@Static Classes@ +=
interface Resolvable {
  StorableValue resolve(Variable var, Register reg);
}
@At Address Members@ +=
public StorableValue resolve(Variable var, Register reg) { return at.equals(var) ? new AtAddress(reg, offset) : this; }
@Variable Members@ += 
public StorableValue resolve(Variable var, Register reg) { return equals(var) ? reg : this; }
@Instruction Members@ +=
protected <E> E resolve(E from, Variable var, Register reg) {
  return from instanceof Resolvable ? (E) ((Resolvable) from).resolve(var, reg) : from;
}
~~~~

We'll iterate through the linear programming solution and resolve variables as needed. For each instruction, we'll use the node before to work out which variables are where. Note that the variable must be live at the previous node, by definition.

~~~~
@Copy Solution Into Code@ +=
for(int i = 0; i < state.code.size(); ++i)
  for(Variable variable : state.code.get(i).uses(Variable.class)) {
    int v = indexOf(controlFlow.variables, variable);
    for (Register r : Register.ASSIGNABLE) {
      int index = Collections.binarySearch(controlFlow.columns, new VarInRegAtInstr(v, r, controlFlow.prevNode[i]));
      if(index >= 0 && glp_mip_col_val(allocation, index + 1) > 0.5) { // huge tolerance for 0-1 integer
        state.code.set(i, state.code.get(i).resolve(variable, r)); break;
      }
    }
  } 
~~~~

~~~~
@Copy Solution Into Code@ +=
Multimap<Integer, Instruction> 
  stackMovesBefore = HashMultimap.create(), 
  stackMovesAfter = HashMultimap.create();
for (int v = 0; v < controlFlow.variables.length; ++v)
  for (int n = 0; n < controlFlow.numNodes; ++n)
    for(Register r : Register.ASSIGNABLE) {
      int index = Collections.binarySearch(controlFlow.columns, new NewVarInRegFlag(v, r, n));
      if(index >= 0 && glp_mip_col_val(allocation, index + 1) > 0.5) // v has moved
        if(r == Register.THESTACK)
          @V Now On Stack@
        else
          @V Now In Register@
    }
~~~~

The != _THESTACK_ check catches newly-initialised variables and dead variables (which are forced onto the stack).

~~~~
@V Now On Stack@ +=
for(int i : controlFlow.instructionsBeforeNode.get(n))
  stackMovesAfter.put(i, move(
    whereAmI(allocation, controlFlow, v, controlFlow.prevNode[i]), 
    state.stackVar(controlFlow.variables[v], new Immediate(8))));
~~~~

~~~~
@V Now In Register@ +=
for(int i : controlFlow.instructionsFollowingNode.get(n))
  stackMovesBefore.put(i, move(
    state.stackVar(controlFlow.variables[v], new Immediate(8)), 
    whereAmI(allocation, controlFlow, v, n)));
~~~~


Inserting is a bit tricky as the indices will change

~~~~
@Insert Stack Moves@ +=
int drift = 0;
for(int i : Sets.newTreeSet(Sets.union(stackMovesBefore.keySet(), stackMovesAfter.keySet()))) {
  state.code.addAll(i + drift, stackMovesBefore.get(i)); 
  drift += stackMovesBefore.get(i).size();
  state.code.addAll(i + drift + 1, stackMovesAfter.get(i)); 
  drift += stackMovesAfter.get(i).size();
}
~~~~

Now we know how many local vars we need, we can reserve the appropriate amount of space. However, we must preserve 16-byte alignment when we're making function calls, so rather than rounding the stack pointer before every function call we'll just over-allocate 8 bytes of stack space.

~~~~
@Parsing State Members@ +=
Immediate staticStackBytes() {
  UnsignedLong ret = stackBytes;
  if(framePointer == null)
    ret = ret.add(UnsignedLong.asUnsigned(mostStackArgs).multiply(SIZE));
  return new Immediate(ret).alignAllocation();
}
@Allocate Stack Space For Variables@ +=
Immediate staticBytes = state.staticStackBytes();
if(!staticBytes.value.equals(ZERO)) {
  state.code.add(1, Op.subq.with(staticBytes, Register.rsp));
  addBeforeRets(state, Op.addq.with(staticBytes, Register.rsp));
}
~~~~

As we're not using _rbp_ to store the stack frame pointer we'll have to adjust the code we need to access a parameter so we subtract the local variables we've allocated, as well as the offset before the stack frame. The code we generate to call functions is important - as we push values onto the stack this offset will increase. We avoid this by calculating all the _Values_ that are going to be passed as parameters first, before pushing them all onto the stack in one go.

## (III) Writing the Output ##

The x86 syntax we'll use: [addl](http://stackoverflow.com/questions/1619131/addl-instruction-x86) [all instructions](http://en.wikipedia.org/wiki/X86_instruction_listings)

~~~~
@Definition x86 Code@ += 
if(isGlobal)
  return String.format( ".globl %s\n%s:", label.name, label.name);
else
  return String.format("%s:", label.name); 
@BinaryOp x86 Code@ += return String.format( "%s %s, %s", op, arg1, arg2);
@Push x86 Code@ += return String.format( "pushq %s", value);
@Pop x86 Code@ += return String.format( "popq %s", value);
@Call x86 Code@ +=
if(name instanceof Label)
  return String.format( "call %s", ((Label)name).name); // call a label
else
  return String.format( "call *%s", name); // indirect function call - code address in a register
@Section x86 Code@ += return String.format( ".%s", name()); 
@StackVar x86 Code@ +=
if(function.framePointer == null) {
  UnsignedLong offset = function.staticStackBytes().value.subtract(bytesIntoStack).subtract(SIZE);
  return String.format("0x%s(%%rsp)", offset.toString(16)); // end of stack frame - stack size + n + return address
} else   
  return String.format("-0x%s(%s)", bytesIntoStack.toString(16), function.framePointer); // start of stack from + n
@Align x86 Code@ += ".align " + SIZE
@Text x86 Code@ += ".text"
@Ret x86 Code@ += "ret"
@Define Label x86 Code@ += return String.format("%s:", label.name);
@Immediate x86 Code@ += return String.format("$0x%s", value.toString(16));
@Parameter x86 Code@ +=
if(function.framePointer == null)
  return new AtAddress(rsp, (number+1)*SIZE.longValue() + function.staticStackBytes().value.longValue()).toString();
else   
  return new AtAddress(function.framePointer, (number+1)*SIZE.longValue()).toString();
@Jump x86 Code@ += return String.format("j%s %s", cond == null ? "mp" : cond, to.name);
@Move x86 Code@ += 
if(cond == null)
  return String.format("movq %s, %s", from, to);
else
  return String.format("cmov%s %s, %s", cond, from, to);
@Label x86 Code@ += return String.format("%s@GOTPCREL(%%rip)", name);
@At Label x86 Code@ += return String.format("%s(%%rip)", at.name);
@At Address x86 Code@ +=
return offset == 0 ? String.format("(%s)", at) : String.format("0x%s(%s)", Long.toString(offset, 16), at);
@Release Args x86 Code@ +=
return Op.addq.with(new Immediate(numArgs * SIZE.longValue()).alignAllocation(), rsp).toString();
@Store Arg x86 Code@ +=
if(function.framePointer == null)
  return move(arg, new AtAddress(rsp, position * SIZE.longValue())).toString();
else
  return new Push(arg).toString();
~~~~

We'll always write to UTF, even if our assembly only ever has ASCII characters in it. We won't bother indenting the code.

~~~~
@Write The Output@ +=
Writer writer = new OutputStreamWriter(asmOut, Charsets.UTF_8); 
if(!program.globals.isEmpty()) {
  writer.append(".data\n");
  for(Entry<Label, Immediate> global : program.globals.entrySet()) 
    writer
      .append(global.getKey().name).append(": .long 0x")
      .append(global.getValue().value.toString(16)).append("\n");
}
writer.append(".text\n");
for(Instruction i : filter(concat(program.text), noNoOps)) 
  writer.append(i + "\n");
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
public abstract boolean isNoOp(Value arg1, StorableValue arg2);
@Move Members@ +=  
public boolean isNoOp() { return from.equals(to); }
@Release Args Members@ +=
public boolean isNoOp() { return function.framePointer == null || numArgs == 0; }
@Zero Is Identity@ +=
public boolean isNoOp(Value arg1, StorableValue arg2) { return arg1.equals(new Immediate(0)); }
@ADDQ@ += @Zero Is Identity@
@SUBQ@ += @Zero Is Identity@
@ORQ@ += @Zero Is Identity@
@All Bits Set Is Identity@ +=
public boolean isNoOp(Value arg1, StorableValue arg2) { return arg1.equals(new Immediate(0xFFFFFFFFFFFFFFFFL)); }
@ANDQ@ += @All Bits Set Is Identity@
@No Identity@ +=
public boolean isNoOp(Value arg1, StorableValue arg2) { return false; }
@CMPQ@ += @No Identity@
~~~~

## Appendices ##

~~~~
@Imports@ += 
import static com.blogspot.remisthoughts.compiletoasm.UnsignedLexer.*;
import static com.blogspot.remisthoughts.compiletoasm.Compiler.Register.*;
import static com.google.common.collect.Iterables.*;
import static com.google.common.collect.Multimaps.*;
import static com.google.common.base.Predicates.*;
import static com.google.common.primitives.UnsignedLong.*;
import static org.gnu.glpk.GLPK.*;
import static org.gnu.glpk.GLPKConstants.*;
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
import org.gnu.glpk.*;
~~~~

Antlr code to build AST
[unicode](http://stackoverflow.com/questions/2081862/how-do-i-match-unicode-characters-in-antlr) [charVocabulary not in 3.3](http://antlr.1301665.n2.nabble.com/UTF-8-charVocabulary-in-options-in-3-3-td7578297.html)

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
      new ANTLRInputStream(srcIn, Charsets.UTF_8.name())))).eval().tree;
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

Some array helpers (we do a lot of linear searching, but not enough to warrant sorting for a binary search or building a hash map).

~~~~
@Other Helpers@ +=
static <T> int indexOf(T[] ts, T t) {
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
llvm-objdump-mp-3.1 -disassemble $(which gcc)  