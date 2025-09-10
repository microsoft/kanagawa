% Mapping To Hardware

This document describes how constructs in the source language map to circuits in
the generated hardware.  Understanding this material is not required for using
Kanagawa, but can be helpful to those with a hardware background.

# Control Flow

## Overview
Like most compilers for imperative languages, the Kanagawa compiler decomposes
source code into basic blocks and branches.  A basic block is a sequence of
statements with the property that if a particular thread executes the first
statement in a block, that thread will execute all statements in the block.  A
branch is a link between basic blocks.  Each basic block corresponds to a
pipeline in the generated hardware. Each branch corresponds to a FIFO in the
generated hardware.

The function below corresponds to a single basic block.  FIFOs hold the function
arguments and the return value.

```
class Foo
{
    uint32 F(uint32 x)
    {
        uint32 y = x + 2;

        uint32 z = y * 10;

        return z / 4;
    }
}
```

<img src="./content/simple-pipeline.svg">

## Threads
Threads in Kanagawa are runtime constructs.  The source does not specify the
number of threads that will be created.  A thread is defined by:

* Context: A value for each local variable
* Location: The next statement that the thread will execute

In the generated hardware, thread context is stored in pipeline registers or
FIFOs. Thread locations change as the thread moves through generated pipelines.
At any particular moment in time, a specific thread exists in either a specific
pipeline stage or in a particular slot in a FIFO.

Threads physically progress through the generated hardware in a way that is analogous to how
threads logically progress through the statements in the source code.  The
following diagram illustrates how a set of threads progress through a function over time.

<img src="./content/threads-move.svg">

## static for
`static for` replicates the body of the `static for` loop. If there are no
data dependencies between loop iterations `static for` translates to a wide
pipeline.

```
uint32[4] x;
uint32[4] y;

static for (const auto i : 4)
{
    y[i] = (x[i] + 2) * 3;
}
```

<img src="./content/unrolled-for-parallel.svg">

The depth of the generated pipeline can increase if there are data dependencies
between loop iterations.

```
uint32[4] x;
uint32[4] y;

// This variable is read and written by each loop iteration
uint32 z = 3;

static for (const auto i : 4)
{
    y[i] = (x[i] + 2) * z;

    z = z + x[i];
}
```

<img src="./content/unrolled-for-serial.svg">

## Conditions

`if/else` and `switch` statements are implemented with multiplexers.

```
uint32 x;
uint32 y;
uint32 z;

if (x > 3)
{
    y = z;
}
else
{
    y = 9;
}
```

<img src="./content/condition.svg">

Multiplexers are only inserted when needed to ensure correctness.
For example, the following code:

```
uint32 x;
uint32 y;
uint32 result = 19;

if (x == 4)
{
    uint32 z = y + 4;
    uint32 w = x * 6;

    result = z - w;
}
```

produces the same hardware as:

```
uint32 x;
uint32 y;
uint32 result = 19;
bool b = (x == 4);

{
    // Note how the variable 'b' is not used to compute z nor w
    uint32 z = y + 4;
    uint32 w = x * 6;

    result = b ? z - w : result;
}
```

## Call Site

Function call sites require a FIFO that saves a per-thread context
at the call site.  In the example below, the local variables that are saved in
the context are `a` and `b`.  This FIFO is only required for function
calls that are *not* inlined by the compiler.
```
class Foo
{
    uint32 G(uint32 x)
    {
        return x + 1;
    }

    uint32 F(uint32 a)
    {
        uint32 b = a + 2;

        uint32 c = G(a + 4);

        return b + c;
    }
}
```

<img src="./content/sync-function-call.svg">

## Multiple Call Sites

Calls to a non-inlined function from multiple call sites are implemented by
adding a FIFO at each call site. An arbiter selects between these FIFOs and
routes values into the entry FIFO for the called function. This arbiter uses
a round-robin arbitration scheme  for each call, except in the case where
the called function has a boolean parameter with the `end_transaction`
attribute.

```
class Foo
{
    uint32 F(uint32 a)
    {
        return G(a + 1);
    }

    uint32 H(uint32 b)
    {
        return G(b + 2);
    }

    uint32 G(uint32 x)
    {
        return 2*x;
    }
}
```

<img src="./content/multiple-call-sites.svg">

## async_exec

A call to `async_exec` is implemented with a single FIFO.  For the common case of
passing a lambda to `async_exec`, the FIFO holds the value of each variable
captured by the lambda.

```
class Foo
{
    void F(uint32 a)
    {
        uint32 b = a + 1;

        async_exec
        (   [b]()
            {
            }
        );
    }
}
```

<img src="./content/async-exec.svg">

## pipelined_for

Each call to `pipelined_for` inserts a record into a FIFO.  This FIFO holds the
thread count and the values of captured variables.  A finite state machine
translates each FIFO record into `thread_count` calls to the inner function.  A
separate finite state machine unblocks the calling thread after `thread_count`
threads have completed.  Unblocking is achieved with zero-width fifo.

```
class Foo
{
    void F(uint32 x, uint32 y)
    {
        uint32 thread_count = x + 1;

        pipelined_for
        (thread_count,
            [y](uint32 tid)
            {
            }
        );
    }
}
```
<img src="./content/pipelined-for.svg">

## pipelined_map

The hardware generated for `pipelined_map` is similar to the hardware generated
for `pipelined_for`.  The thread collection finite state machine concatenates
the return values from each inner thread into an array and returns that array to
the caller.

```
class Foo
{
    uint32[4] F(uint32 x, uint32 y)
    {
        uint32 thread_count = x + 1;

        uint32[4] result = pipelined_map<4>
        (thread_count,
            [y](uint32 tid) -> uint32
            {
                return y + tid;
            }
        );

        return result;
    }
}
```
<img src="./content/pipelined-map.svg">

## pipelined_last

The hardware generated for `pipelined_last` is similar to the hardware generated
for `pipelined_for`.  The thread collection finite state machine ignores the
return value from all inner threads except the last one (where `tid ==
(thread_count - 1)`).  This value is returned to the caller.  If the thread
count is 0, then the value returned by `pipelined_last` is undefined.

```
class Foo
{
    uint32 F(uint32 x, uint32 y)
    {
        uint32 thread_count = x + 1;

        uint32 result = pipelined_last
        (thread_count,
            [y](uint32 tid)
            {
                return y + tid;
            }
        );

        return result;
    }
}
```
<img src="./content/pipelined-last.svg">

## atomic do
`atomic do` is implemented by storing a thread's context (local variables) into a FIFO.  A
finite-state machine evaluates the loop body and condition of the `atomic do` (for one thread) at each
clock cycle.  When the loop condition evaluates to `false` a thread context is
popped off of the FIFO and the corresponding thread continues execution.

```
class Foo
{
    // shared variable
    uint32 count = 0;

    uint32 F(uint32 x)
    {
        uint32 y = x + 4;

        atomic do {} while (count < x);

        return x + y;
    }
}
```
<img src="./content/wait-for.svg">

## Sequential Loops

A mux exists before the body of a `for` or `do/while` loop that select threads
to enter the loop body from two sources:

* The backwards link FIFO (threads that have finished loop iteration `N` and are waiting
  to execution iteration `N+1`)
* The loop entry FIFO (threads that have never executed the loop body)

The mux arbitration policy is as follows:

* If only one of the two inputs sources contains a thread ready to execute the
  loop body, then that source is selected.
* If both input sources contain a thread ready to execute the loop body, then
  the backwards link is chosen.

When threads complete an iteration, the loop condition is evaluated.  If the
condition evaluates to `false`, then the thread follows the backwards link to
execute again.  If the condition evaluates to `true`, then the thread exits the
loop via a FIFO or reorder buffer.

The loop exit structure is a FIFO when the loop does not need to preserve
ordering (for example, loops with the `[[unordered]]` attribute).  The loop exit
structure is a reorder buffer when thread ordering should be preserved.  This
ensures that a set of threads execute the code after the loop in the same order that
the set of threads executed the code before the loop.

```
class Foo
{
    void F()
    {
        bool done = false;

        do
        {

        } while (!done);
    }
}
```

<img src="./content/do-while.svg">

## reorder
The `reorder` block can be used to reorder the results from an unordered loop or a call to a an unordered function.

```
class Foo
{
    [[pipelined]] uint32 F(uint32 a)
    {
        uint32 result;

        reorder
        {
            result = G(a);
        }

        return result;
    }

    [[unordered]] uint32 G(uint32 count)
    {
        uint32 result = 0;

        [[unordered]] for (const uint32 i : count)
        {
            result += 1;
        }

        return result;
    }
}
```

<img src="./content/reorder-block.svg">


## Composition

The control flow constructs in the Kanagawa language compose.  For example, you
can nest control flow constructs within others.  The hardware generated by
Kanagawa follows a composition that is isomorphic to the control flow
composition in the source.  For example, the following code and diagrams
illustrate a `atomic do` loop nested within a `do/while` loop.

```
class Foo
{
    uint32 count = 0;

    void F()
    {
        bool done = false;
        uint32 x;

        do
        {
            atomic do {} while (count < x);
        } while (!done);
    }
}
```

<img src="./content/wait-for-do-while.svg">

# Data

## Local Variables
As the following diagram illustrates, the values of local variables are stored
in pipeline registers and/or FIFOs:

```
class Foo
{
    void F(uint32 x, uint16 y)
    {
        uint32 z = x * 15;

        uint48 w = z + y;

        async_exec([w]()
        {
            uint64 a = w + 3;
        });
    }
}
```

<img src="./content/local-variables.svg">

## Arrays

Array accesses have dramatically difference resource and time costs depending if
the value of the index is known at compile time.  If an array index is known at
compile time, then array indexing has no cost.

Array reads with a dynamic index are implemented with a multiplexer:

```
uint32[4] x;
index_t<4> i; // not known at compile time
uint32 y;

y = x[i];

// The assignment above is the same as:
y = mux(i, x[0], x[1], x[2], x[3]);
```
<img src="./content/array-read.svg">

Array writes with a dynamic index are implemented with a de-multiplexer:

```
uint32[4] x;
index_t<4> i; // not known at compile time
uint32 y;

x[i] = y;

// The assignment above is the same as:
static for (const auto j : 4)
{
    if (i == j)
    {
        // j is known at compile time
        x[j] = y;
    }
}
```
<img src="./content/array-write.svg">

## Memory

A memory in the Kanagawa language corresponds to a logical memory in the
generated hardware with `R` read ports and `W` write ports.  `R` is equal to the
number of statements in the source that read from the memory.  `W` is equal to
the number of statements in the source that write to the memory.  Note that the
counting of statements occurs after `static for` loops have been unrolled and
function calls have been inlined.

Each logical memory is implemented with a set of physical memories.

### Read Ports

By default, there exists a unique replica of the memory contents for each read
port.  If a memory has the `[[non_replicated]]` attribute then all read ports
share a single replica.  If there are concurrent reads, then an arbiter grants
access to a single port on each cycle (and returns undefined values to all other
ports).

### Write Ports

If there are multiple write ports for a given memory then an arbiter is
instantiated that grants write access to a single write statement on each cycle,
and drops write requests from other ports.

### Examples

#### 2R2W memory
This code and diagram illustrates the implementation of a memory with 2 write ports and 2
read ports.
```
class Foo
{
    memory<uint32, 256> mem;

public:
    void F(uint8 i, uint32 val)
    {
        // write port 1
        mem[i] = val;
    }

    void G(uint8 i, uint32 val)
    {
        // write port 2
        mem[i] = val + i;
    }

    uint32 H(uint8 i)
    {
        // read port 1
        return mem[i];
    }

    uint32 J(uint8 i)
    {
        // read port 2
        return mem[i + 2];
    }
}
```

<img src="./content/memory.svg">

#### 2R2W non-replicated memory
This code and diagram illustrates the implementation of a non-replicated memory
with 2 write ports and 2 read ports.
```
class Foo
{
    memory_norep<uint32, 256> mem;

public:
    void F(uint8 i, uint32 val)
    {
        // write port 1
        mem[i] = val;
    }

    void G(uint8 i, uint32 val)
    {
        // write port 2
        mem[i] = val + i;
    }

    uint32 H(uint8 i)
    {
        // read port 1
        return mem[i];
    }

    uint32 J(uint8 i)
    {
        // read port 2
        return mem[i + 2];
    }
}
```

<img src="./content/memory-norep.svg">

# Reset

The reset pin of generated RTL from Kanagawa design is synchronous to the clock. The reset input is
synchronous to the clock.
