### my file ###
add some *text*:

~~~~~
@*@ +=
int main() { 
  @Do Stuff@
  @Do More Stuff@
  return 0; 
}
~~~~~

and some more text. we should probably say what dostuff is; it's:

~~~~~
@Do Stuff@ +=
	int* me = &1;
	*me++;
@Do More Stuff@ +=
/* MORE! MORE! */
@Do Stuff@ +=
// again!
~~~~~

was that clearer? More stuff is the same as the first:

~~~~
@Do More Stuff@ += @Do Stuff@
~~~~