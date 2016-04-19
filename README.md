# Hundred Pacer

![A sharp-nosed viper](https://upload.wikimedia.org/wikipedia/commons/thumb/b/b7/Sharp-Nosed_Viper_01.jpg/1920px-Sharp-Nosed_Viper_01.jpg)

> The popular name “hundred pacer” refers to a local belief that,
> after being bitten, the victim will only be able to walk 100 steps
> before dying.

This compiler tries to run in as few steps as possible – programs
may be able to finish within 100 paces.

## Language

The language is identical to Diamondback.

## Implementation

There are several differences in the implementation.  First, it compiles to
_64_-bit x86 instructions.  This part of the transformation has been provided
for you, but it has several consequences.

### Tail Calls

This implementation supports _tail calls_, so you can run programs like:

```
def f(x, t):
  if x < 1: t
  else: f(x - 1, x + t)

f(888888888, 0)
```

without running out of stack space.  This is mainly so you can test
long-running programs; you can study the implementation of tail calls across
the `CApp` and `ADFun` cases in the compiler if you want to understand the
details of the approach taken.

### Registers

x86\_64 is useful because it gives us 8 more registers (named `R1`, `R2`, and
so on up to `R8`).

The registers are used in the following way (note that `R` is used in place of
`E` to refer to the registers in 64-bit assembly).

```
RAX  : Return values and "scratch work"
RCX  : "Scratch work"

RSP  : Stack pointer
RBP  : Base pointer

RDI  : Argument 1 to C function calls

R1-8 : Local variables
```

The main change you will deal with is that local variables are stored in
registers `R1-R8` as much as possible.  The details are below.

### Using Registers for Variables

So far, we've allocated stack space for every variable in our program.  It
would be more efficient to use registers when possible.  Take this program as an
example:

```
let n = 5 * 5 in
let m = 6 * 6 in
let x = n + 1 in
let y = m + 1 in
x + y
```

In the main body of this program, there are 4 variables – `n`, `m`, `x`, and
`y`.  In our compiler without register allocation, we would assign these 4
variables to 4 different locations on the stack.  It would be nice to assign
them to registers instead, so that we could generate better assembly.  Assuming
we have 4 registers available, this is easy; we could pick

- `n` ↔`R1`
- `m` ↔`R2`
- `x` ↔`R3`
- `y` ↔`R4`

```
mov R1, 10 ; store the result for n in R1 directly
sar R1, 1
mul R1, 10
mov R2, 12 ; store the result for m in R2 directly
sar R2, 6
mul R2, 12
mov R3, R1 ; store the result for x in R3 directly
add R3, 2
mov R4, R2 ; store the result for y in R4 directly
add R4, 2
mov RAX, R3 ; store the answer in RAX directly (our new RAX)
add RAX, R4
```

This avoids four extra `mov`s into memory, and allows us to use registers
directly, rather than memory, as we would have in the initial verson:

```
mov RAX, 10
sar RAX, 1
mul RAX, 10
mov [RBP-8], RAX ; extra store
mov RAX, 12
sar RAX, 6
mul RAX, 12
mov [RBP-16], RAX ; extra store
mov RAX, [RBP-8] ; memory rather than register access
add RAX, 2
mov [RBP-24], RAX ; extra store
mov RAX, [RBP-16] ; memory rather than register access
add RAX, 2
mov [RBP-32], RAX ; extra store
mov RAX, [RBP-24] ; memory rather than register access
add RAX, [RBP-32] ; memory rather than register access
```

Making this change would be require a few alterations to the compiler:

1. We'd need to have our environment allow variables to be bound to
   _registers_, rather than just a stack offset.
2. We need to change the goal of the compiler from “get the answer into RAX”
   to “get the answer into <insert location here>”
3. Now, whenever we call a function, that function may overwrite the values of 
   registers the current context is using for variables.  This demands that we
   save the contents of in-use registers before calling a function.

To handle (1), we define a new datatype, called `location`:

```
data location
  | LReg of reg
  | LStack of int
```

And we change the compiler's `env` parameter to have type `(location envt)`: a
map from strings to `location`s, which can be either registers or stack offsets.

To handle (2), we add a new parameter to the compiler, which we'll call `into`,
of type `location`, which is where to store the result of the computation.  For
function bodies and main, this will be `LReg(RAX)`, so that return values are
handled in the usual way.  For let bindings, we will use the `location` in the
given environment to choose the `into` parameter for the binding part of the
call.

There are a few options for handling (3):

- We could have each function save and restore all of the registers it uses, so
  callers do not have to store anything.
- We could have each caller store the registers in use in its context 
- We could blend the first two options, which gives us the notion of
  caller-save vs. callee-save registers

The first option is the simplest so it's what the compiler does. It requires
one wrinkle – when calling an _external_ function like `print`, we need to
save all the registers the current function is using.  The current
implementation simply stores all the registers in `env` by pushing and popping
their values before and after the call.  It's interesting (though not part of
the assignment) to think about how we could do better than that.

These changes have been mostly made for you, but you will need to understand
them so your register allocator (below) can interface correctly with the
compiler.  In particular, you will need to calculate the set of live variables
for each function call (each `CApp`) so that the correct registers can be
saved and restored.

### Register Allocation

For programs that use fewer variables than the number of available registers,
the strategy above works well.  This leaves open what we should do if the
number of variables _exceeds_ the number of available registers.

An easy solution is to put N variables into N registers, and some onto the
stack.  But we can do better in many cases.  Let's go back to the example
above, and imagine that we only have _three_ registers available, rather than
four.  Can we still come up with a register assignment for the four variables
that works?

The key observation is that once we compute the value for `x`, we no longer
need `n`.  So we need space for `n`, `m`, and `x` all at the same time, but
once we get to computing the value for `y`, we only need to keep track of `m`
and `x`.  That means the following assignment of variables to registers works:

- `n` ↔`R1`
- `m` ↔`R2`
- `x` ↔`R3`
- `y` ↔`R1`

```
mov R1, 10 ; store the result for n in R1 directly
sar R1, 1
mul R1, 10
mov R2, 12 ; store the result for m in R2 directly
sar R2, 6
mul R2, 12
mov R3, R1 ; store the result for x in R3 directly
add R3, 2
mov R1, R2 ; store the result for y in R1, overwriting n, which won't be used from here on
add R1, 2
mov RAX, R3 ; store the answer in RAX directly (our new RAX)
add RAX, R1 ; R1 here holds the value of y
```

It was relatively easy for us to tell that this would work.  Encoding this idea
in an algorithm—that multiple variables can use the same register, as long as
they aren't in use at the same time—is known as _register allocation_.

One way to understand register allocation is to treat the variables in the
program as the vertices in a (undirected) graph, whose edges represent
dependencies between variables that must be available at the same time.  So,
for example, in the picture above we'd have a graph like:

```
n --- m
  \ / |
   ╳  |
  / \ |
y --- x
```

If we wrote an longer sequence of lets, we could see more interesting graph
structures emerge; in the example below, `z` is the only link between the first
4 lets and the last 3.

```
let n = 5 * 5 in
let m = 6 * 6 in
let x = n + 1 in
let y = m + 1 in
let z = x + y in
let k = z * z in
let g = k + 5 in
k + 3
```

```
n --- m
  \ / |
   ╳  |
  / \ |
y --- x
|   /
|  /
| /
z --- k --- g

```

Here, we can _still_ use just three registers.  Since we don't use `x` and `y`
after computing `z`, we can reassign their registers to be used for `k` and
`g`.  So this assignment of variables to registers works:

- `n` ↔`R1`
- `m` ↔`R2`
- `x` ↔`R3`
- `y` ↔`R1`
- `z` ↔`R2`
- `k` ↔`R1`
- `g` ↔`R3`

(We could also have assigned `g` to `R2` – it just couldn't overlap with `R1`,
the register we used for `k`.)

Again, if we stare at these programs for a while using our clever human eyes
and brains, we can generate these graphs of dependencies and convince ourselves
that they are correct.

To make our compiler do this, we need to generalize this behavior into an
algorithm.  There are two steps to this algorithm:

1. Create the graph
2. Figure out assignments given a graph to create an environment

The second part corresponds directly to a well-known (NP-complete) problem
called [Graph Coloring](https://en.wikipedia.org/wiki/Graph_coloring).  Given a
graph, we need to come up with a “color”—in our case a register—for each
vertex such that no two adjacent vertices share a color.  We will use a library
([ocamlgraph](http://ocamlgraph.lri.fr/index.en.html)) to handle this for us.

The first part is an interesting problem for us as compiler designers.  Given
an input expression, can we create a graph that represents all of the
dependencies between variables, as described above?  If we can, we can use
existing coloring algorithms (and fast heuristics for them, if we don't want to
wait for an optimal solution).  This graph creation is the fragment of the
compiler you will implement in this assignment.

In particular you'll implement two functions (which may each come with their own helpers):

```
(* Given an expression, return a list of dependency edges between
variables *)
dep_graph :: (ae : aexpr) -> (string * string) list

(* Given a list of registers, a set of variables, and a set of edges,
create an environment of locations that uses as many registers as
possible *)
get_colors :: (regs : reg list) (varlist : string list) (edges : (string * string) list) : location envt =

(* combine dep_graph and get_colors to create an environment for the given expression *)
colorful_env :: (ae : aexpr) -> location envt
```

In class, this helper signature was suggested for doing the work of
`dep_graph`:

```
(*

actives is a list of active variables from outside the expression
(for example in a nested if).

The return includes both the active variables from this expression,
and the list of edges that this expression creates

*)
dep_graph_ae :: (ae : aexpr) (actives : string list) -> (string list * (string * string) list)
dep_graph_ce :: (ce : cexpr) (actives : string list) -> (string list * (string * string) list)
```


### Testing

There are two new useful mechanisms for testing.

- `tdep` takes a program (as a string) and a list of edges and checks that
  calling `dep_graph` on the main body produces the given edges.

- `tcolor` takes a program (as a string) and a number, and checks that calling
  `get\_colors` coloring the dependency graph with (exactly) the given number
  of colors.  This is a useful way to check that you are calling the coloring
  function correctly after generating dependencies.

You can also use `t` as usual, to test that programs run as expected; it will
run with 8 available registers (`R1-R8`).

You can also compile programs with differing numbers of registers available by
setting the `NUMREGS` variable.  So, for example, you can create an input file
called `input/longloop.diamond`, populate it with the long loop at the
beginning of the writeup above, and run:

```
$ make output/longloop.run NUMREGS=3
```

And this will trigger the build for `longloop` with just 3 registers.  This can
be fun for testing the performance of long-running programs with different
numbers of registers available.  Setting `NUMREGS` to `0` essentially emulates
the performance of our past compilers, since it necessarily allocates all
variables on the stack.


