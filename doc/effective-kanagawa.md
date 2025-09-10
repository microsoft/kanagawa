% Effective Kanagawa

# Correctness

## Shared state
This section lists common idioms for accessing shared state.  The list is
sorted by complexity (or verification effort required).  Strive to write most
Kanagawa code using low complexity idioms.

### No shared state
Kanagawa code that only accesses local variables is the lowest risk.  An added
benefit of this pattern is that it allows high clock frequencies to be achieved
because constructs like `atomic`, `[[schedule(n)]]`, `atomic do` are not needed.

```
inline uint32 F(uint32 a, uint32 b)
{
    return a + b;
}
```

### Shared variables exclusive to one atomic block
The fool-proof way to use a shared variable is to access that shared variable
from only 1 `atomic`/`[[schedule(N)]]` block.  One way to ensure this is to
declare a static local variable within an atomic block.  Using this pattern
ensures a compile-time error if the shared variable is accessed outside of the
block where it is declared.

A typical use case for this pattern is when multiple threads all execute a
common code path.

```
inline uint32 Accumulate(uint32 a, bool reset)
{
    uint32 result;

    atomic
    {
        // Shared variable, only visible within the atomic block
        static uint32 _sum = 0;

        _sum = reset ? a : _sum + a;
        result = _sum;
    }

    return result;
}
```

This common idiom is abstracted by the function `attomically` defined in the
standard library module `sync.atomic`.

```
import sync.atomic

inline uint32 Accumulate(uint32 a, bool reset)
{
    const auto x = atomically([a, reset](uint32 x) -> uint32
    {
        return reset ? a : x + a;
    });

    return x.second;
}
```

Verification considerations:

* Is the initial value of the shared variable correct?  Remove the initial value
  and verify that tests fail.
* Have all control signals (like `reset` above) been tested?
* For `[[schedule(N)]]` blocks, is the value of `N` correct?  Increase the value
  of `N` and verify that tests fail.

### Shared variable with 1 write location
If a shared variable must be accessed outside of a single `atomic`/`[[schedule(N)]]`
block, try to design the code such that there is only one statement that writes to
the shared variable.

```
class Accumulator
{
private:
    uint32 _sum = 0;

public:
    void Accumulate(uint32 x, bool reset)
    {
        atomic
        {
            // The only place where _sum is written
            _sum = reset ? x : _sum + x;
        }
    }

    uint32 GetResult()
    {
        return _sum;
    }
}
```

Verification considerations:

* Do writes to the shared variable need to be placed in `atomic` blocks?  If
  multiple shared variables are written at nearby lines, most likely the writes should be contained
  within an `atomic` block.
* Do reads of the shared variable need to be placed in `atomic` blocks?  If
  multiple shared variables are read in nearby lines, most likely the reads
  should be contained within an `atomic` block.
* What synchronization is needed between the write and read locations?
* For non-replicated memories: can a data race occur (multiple concurrent reads)?
* Is the initial value of the shared variable correct?  Remove the initial value
  and verify that tests fail.
* Have all control signals (like `reset` above) been tested?
* For `[[schedule(N)]]` blocks, is the value of `N` correct?  Increase the value
  of `N` and verify that tests fail.

### Shared variable with multiple write locations
The most complex code contains multiple statements that each can write to a
shared variable.  Read the following code an convince yourself that a data race
is impossible (two threads writing to `_flag` concurrently).

```
inline void F()
{
    // Shared variable, with multiple write locations
    static bool _flag = false;

    bool local_flag;

    atomic
    {
        local_flag = _flag;

        if (!_flag)
        {
            // write location 1
            _flag = true;
        }
    }

    // more code here

    atomic
    {
        if (!local_flag)
        {
            // write location 2
            _flag = false;
        }
    }
}
```

Verification considerations:

* Can a data race occur (concurrent writes to the same shared variable)?
* Do writes to the shared variable need to be placed in `atomic` blocks?
* Do reads of the shared variable need to be placed in `atomic` blocks?
* What synchronization is needed between the write and read locations?
* For non-replicated memories: can a data race occur (multiple concurrent reads)?
* Is the initial value of the shared variable correct?  Remove the initial value
  and verify that tests fail.
* Have all control signals been tested?
* For `[[schedule(N)]]` blocks, is the value of `N` correct?  Increase the value
  of `N` and verify that tests fail.

## Threading

### Computing number of threads for a vectorized loop
If `item_count` items must be processed by a loop (such as a call to `pipelined_for`)
which operates on `M` elements per iteration, use code like the following to
compute the number of loop iterations and the number of items processed by the
final iteration.

```
import numeric.int.operator

const auto M = 4;

inline void f(uint32 item_count)
{
    auto iter = ceil_div_mod<1>(item_count, M);

    // iter.first contains the number of loop iterations
    // iter.second contains the number of items processed by the last loop iteration
}
```

# Compiler Settings
The following command line parameters trade compile time for quality of generated hardware.

|Switch|Description|Reasonable value for fast compilation|Reasonable value for high quality|
|---|---|---|---|
|`--optimize=N`| Optimization level | 1 | 2 |
|`--work-list-size=N`| Optimization working set limit | 16 | 20 |
|`--schedule-limit=N`| Iterations spent in scheduling to reduce pipeline register count | 100 | 1000 |
|`--max-register-ratio=N`| Max register ratio to try in atomic blocks | `register-ratio` | 2 * `register-ratio` |

## Max register ratio
The `--max-register-ratio` setting allows the compiler to use a higher register
ratio within `[[schedule(N)]]` blocks to meet the schedule constraint while
better dividing logic across pipeline stages. Increasing the difference between
`--register-ratio` and `--max-register-ratio` adds extra iterations to the
compiler scheduling, increasing compiler execution time.

## Optimizing Output For Specific Downstream Tools
The `--sparse-reg-opt` command line switch causes the compiler to
remove bits from pipeline registers that are known to be unused or constant.
Some synthesis tools can perform a similar optimization.

## Multicore compilation
The `jobs=N` command line option controls the number of parallel jobs the
compiler spawns. Generally the value should not be set higher than number of
logical processors, which is the default, but in some cases compilation might
run faster with fewer jobs, especially if the machine has a lot of cores and/or
there is not enough compilation parallelism in the project to exploit.

The compiler can better exploit compilation parallelism if the design logic is
spread across multiple files (modules). This is a good practice for organizing
projects not just to optimize compilation speed but also for general code
maintainability.

# Area

## Minimize Basic Block Count

A basic block is a sequence of statements with the property that if a
given thread executes the first statement of a basic block, that thread will
execute all statements in the basic block.  In general, try to minimize the
total number of basic blocks in a design.  This avoids fixed overhead associated
with each basic block, and makes compiler optimizations more effective.  Static
analysis can be used to inspect the set of basic blocks used to implement a
particular design.

The following control flow constructs do _not_ introduce new basic blocks:

* `static for`
* `if`/`else`
* `switch`
* Inline function calls

### atomic do
`atomic do` usually introduces a new basic block (which contains the `atomic do`
loop body, loop condition, and the statements following `atomic do`.)  This additional basic
block can be avoided by hoisting the `atomic do` statement to the top of the
containing function.

Original:

```
inline void F(uint32 x)
{
    static uint32 _counter;

    // This line forces the atomic do
    // to introduce a new basic block
    uint32 y = x + 2;

    atomic do {} while (x > _counter);
}
```

Optimized:

```
inline void F(uint32 x)
{
    static uint32 _counter;

    atomic do {} while (x > _counter);

    uint32 y = x + 2;
}
```

A common pattern to enable hoisting `atomic do` statements is to split `atomic do` blocking into a separate method:

Original:

```
import data.counter

class IntegerAllocator
{
private:
    counter<32, 32> _free_count;
    uint32 _id = 0;

    inline bool wait_internal()
    {
        bool result = false;

        if (_free_count.count() > 0)
        {
            result = true;
            _free_count.add(1);
        }

        return result;
    }

public:
    uint32 Allocate()
    {
        // Wait for a free slot
        atomic do {} while (!wait_internal());

        uint32 result;

        atomic
        {
            result = _id;
            _id++;
        }

        return result;
    }
}

inline uint32 F(uint32 a)
{
    static IntegerAllocator _allocator;

    uint32 z = a + 3;

    uint32 x = _allocator.Allocate();

    return x + z;
}

```

Optimized:

```
import data.counter

class IntegerAllocator
{
private:
    counter<32, 32> _free_count;
    uint32 _id = 0;

    inline bool wait_internal()
    {
        bool result = false;

        if (_free_count.count() > 0)
        {
            result = true;
            _free_count.add(1);
        }

        return result;
    }

public:
    void Reserve()
    {
        // Wait for a free slot
        atomic do {} while (!wait_internal());
    }

    // Caller must first call Reserve
    uint32 Allocate()
    {
        uint32 result;

        atomic
        {
            result = _id;
            _id++;
        }

        return result;
    }
}

inline uint32 F(uint32 a)
{
    static IntegerAllocator _allocator;

    // Reserve early, to minimize basic block count
    _allocator.Reserve();

    uint32 z = a + 3;

    uint32 x = _allocator.Allocate();

    return x + z;
}
```

### Use inline functions
Calls to inline functions do not introduce a new basic block.  In contrast, non-inline
functions with multiple call sites do introduce multiple basic blocks.  By default,
declare functions as inline.  Non-inline functions should be reserved for
functions that write to shared state (where inlining would introduce data
races).  In the example below, if `Add` was declared inline, and `Add` was
called concurrently from 2 separate call sites, then a data race could occur
that would cause `_sum` to contain an incorrect value.

```
class A
{
private:
    uint32 _sum = 0;

public:
    void Add(uint32 a)
    {
        atomic
        {
            _sum += a;
        }
    }
}
```

If a function has only 1 call site, the inline modifier has no effect as the
compiler will automatically inline the function.

## Minimize Variable Lifetime
The cost of a local variable is equal to the width of that variable (in bits)
times the number of pipeline stages that variable is live for.  Minimizing
either term in this product can reduce area.  The widths of thread ID variables
with `pipelined_*` functions are especially important as some buffers are sized
based on the width of the thread ID.

### Short Circuit
If a variable is written in one location and then read in a small number of
locations, it can be advantageous to store the variable in a memory.

Original:
```
class C
{
    void F(uint6 tid, uint32 x)
    {
        uint32 y = x + 2345;

        pipelined_for(128, [y](uint7 z)
        {
            // many statements
            uint32 w = y + z;
        });
    }
}
```

Optimized:
```
class C
{
    memory<uint32, 64> _mem;

    void F(uint6 tid, uint32 x)
    {
        uint32 y = x + 2345;
        _mem[tid] = y;

        pipelined_for(128, [tid](uint7 z)
        {
            // many statements
            uint32 y = _mem[tid];

            uint32 w = y + z;
        });
    }
}
```

If the definition and the use of the variable are contained with the same function, the compiler
will perform this optimization automatically.

### Re-compute
Another way to reduce the cost of a wide local variable is re-compute it
on-demand based on the values of narrower local variables.  This is similar to
[Hoisting](#hoisting).

Original:
```
inline void F(uint6 tid, uint8 x)
{
    uint32 y = x * 123456;

    // many statements, including uses of y

    // 32-bit 'y' value is captured here
    pipelined_for(128, [y](uint7 z)
    {
        // many statements
        uint32 w = y + z;
    });
}
```

Optimized:
```
inline void F(uint6 tid, uint8 x)
{
    uint32 y = x * 123456;

    // many statements, including uses of y

    // 8-bit 'x' value is captured here
    pipelined_for(128, [x](uint7 z)
    {
        uint32 y = x * 123456;

        // many statements
        uint32 w = y + z;
    });
}
```

## Avoid sequential loops
While Kanagawa supports sequential loops (`for` and `do/while`) other
constructs are usually more efficient.  Sequential loops have a non-trivial area
overhead and do not introduce any new concurrency.  Where possible, call a
`pipelined_*` function or use an `static for` loop.  Reserve usage of
sequential loops for cases where all of the following holds:

* Each loop iteration depends on a previous iteration
* The loop-carried dependency cannot be resolved with `atomic`/`[[schedule(N)]]`
* An `static for` loop cannot because used (loop bound not known at compile
  time, or the area cost of replicated logic is too high)

### Sequential loop reorder buffer
For sequential loops, the default behavior is that threads that finish all loop
iterations will be held in a buffer and are not allowed to move past the loop
until all preceding threads have moved past the loop.

The `[[reorder_by_looping]]` attribute will maintain this ordering of threads but
the ordering is maintained by having threads execute extra iterations of the
loop. The extra iterations are predicated to prevent side effects. This reduces
area by removing the reorder buffer but can reduce throughput because "finished"
threads are still executing the loop.

```
inline void F()
{
    uint32 x;
    [[reorder_by_looping]]
    do
    {
        x++;
    } while (x < 16);
}
```

The `[[unordered]]` attribute can also be used to remove the reorder buffer
but this removes the guarantee about thread ordering when moving past the loop.
See [Thread ordering](programming-guide.md/#thread-ordering) for more details.

## Unrolled loop-carried dependencies
Dependencies between iterations of `static for` loops can result in a deep
pipeline which will increase resource usage.  For example, in the code below the
depth of the generated pipeline is linearly proportional to `N`:

```
const auto N = 8;
uint32[N] input;

uint32 total = 0;

static for (const auto i : N)
{
    total += input[i];
}
```

Consider using library functions that are coded specially to reduce pipeline
depth for cases like this.  Relevant functions to consider are:

`reduce`
`map_reduce`
`inclusive_scan`
`prefix_sum`

## Hoisting

### static for
Hoisting loop-invariant code outside of a loop is common practice in many
languages. In Kanagawa, hoisting loop-invariant code outside of an
`static for` loop will result in an area savings (and in many cases the
compiler will perform this transformation automatically).  For example, the
second `static for` loop below will consume fewer resources because the term
`(x * y) > 4)` is not replicated for each loop body.

```
uint32 x;
uint32 y;
uint32[8] input;

uint32 sum = 0;

static for (const auto i : 8)
{
    sum += (x * y) > 4 ? input[i] : 0;
}

bool cond = (x * y) > 4;

static for (const auto i : 8)
{
    sum += cond ? input[i] : 0;
}
```

### Sequential loops and pipelined_* functions
For sequential loops and calls to `pipelined_*` functions, hosting
loop-invariant code outside of the loop does not reduce resource usage.  The
hardware corresponding to the loop-invariant code consumes the same amount of
resources when it is inside the loop as when it is outside of the loop.  In the
example below, the first call to `pipelined_for` captures 263 bits of local
variables whereas the second call to `pipelined_for` captures 16 bits of local
variables.  In this case, hoisting the shift operation actually results in
higher resource consumption.

```
uint8 x;
uint8 y;

// x << y could potentially require 263 bits to represent
uint263 z = x << y;

pipelined_for(
    32,
    [z](index_t<32> i)
    {
        // do something with z
    });

pipelined_for(
    32,
    [x, y](index_t<32> i)
    {
        uint263 z = x << y;
        // do something with z
    });
```

## Reduce Mux Widths
The bit-width of muxes (e.g., if/else statements or ternary operators) can
sometimes be reduced through refactoring. The following code samples show
examples of this.

Original:

```
bool condition;
uint32 counter;
if (condition)
{
    counter++;
}
```

Optimized:

```
bool condition;
uint32 counter;
uint1 increment = cast<uint1>(condition);
counter += increment;
```
In the original version, the mux is 32 bits wide. In the optimized version, the mux is only 1 bit wide.

Similarly, the following reduces a 32-bit mux to a 2-bit mux.

Original:

```
bool condition;
uint32 counter;
counter = condition ? counter + 1 : counter + 2;
```

Optimized:

```
bool condition;
uint32 counter;
count_t<2> increment = condition ? 1 : 2;
counter += increment;
```

## Optionals
If an `optional` is only used when `is_valid` is true, then setting a default for `value` can avoid an extra mux.

Original:

```
bool condition;
uint32 value;
optional<uint32> x = {};
if (condition)
{
    x = make_optional(true, value);
}
```

Optimized:

```
bool condition;
uint32 value;
optional<uint32> x = make_optional(false, value);
if (condition)
{
    x.is_valid = true;
}
```
In the original version, `x.value` can be either 0 or `value`, thus requiring a
mux. In the optimized version, it is always set to `value`.

## Remove initialization of static variables
Removing initialization of static variables removes the need for reset logic.

Original:

```
inline uint32 F(bool start, bool end)
{
    uint32 result;

    atomic
    {
        // Initialize counter to 0
        static uint32 counter = 0;

        result = counter;

        // Reset counter when end = true
        counter = end ? 0 : counter + 1;
    }

    return result;
}
```

Optimized:

```
inline uint32 F(bool start, bool end)
{
    uint32 result;

    atomic
    {
        // No initialization
        static uint32 counter;

        // Reset counter when start = true
        counter = start ? 0 : counter + 1;

        result = counter;
    }

    return result;
}
```


## Narrow/rotate arrays before indexing
Creating temporary, narrowed, copies of an array prior to indexing can
cause smaller muxes to be created and hence save area.
Rotating the contents of this array can also simplify any mux selection
logic to further save area.

Original:

```
uint8[31] input;
uint8[16] output;
index_t<16> offset;

static for(const auto i : 16)
{
    output[i] = input[i + offset];
}
```

Optimized:

```
uint8[31] input;
uint8[16] output;
index_t<16> offset;

static for(const auto i : 16)
{
    uint8[16] tmp;
    static for(const auto j : 16)
        tmp[j] = input[i + j];
    output[i] = tmp[offset];
}
```

In this optimized case, instead of creating 8-bit 31:1 muxes these are
now reduced to 8-bit 16:1 muxes, and in doing so, rotating their contents has
also caused the 15 adders used to compute i + offset to be eliminated.

Benefits from rotation can be applied independently to narrowing, as would be the
case if `uint8[16] input;` existed in the original example above.
Alternatively, the `shift_array_left`, `shift_array_right`, `rotate_array_left`
and `rotate_array_right` functions from the library module `data.array` can be
used:

```
import data.array

inline void F()
{
    uint8[31] input;
    uint8[16] output;
    index_t<16> offset;

    output = rotate_array_right<16>(input, offset);
}
```

## Memory Allocator
The compiler assigns memories in the source code to physical memory types in the
generated hardware.  A number of compiler parameters control this allocation
process.  These parameters can cause interesting results like a memory with
32-elements being assigned to a block ram with capacity for 512 elements.

These parameters constrain total utilization of a give memory type:

* `--max-block-ram`
* `--max-deep-ram`

If one of these values is not specified, then the maximum number of resources on the
target chip is used instead.

These parameters constrain utilization of specific memory instances:

* `--block-ram-util-threshold`
* `--deep-ram-util-threshold`

These parameters are interpreted as percentages.  For example if
`--block-ram-util-threshold=30`, then no particular block ram will be instantiated if
that instance would have a utilization of less than 30%.

## Trade frequency for area
There are a number of compiler switches that enable area to be reduced at the expense of frequency.

|Switch |Description  | Adjustment to lower area|
|--- | --- | ---|
|`--additional-latency`| Pipelining of control signals | Decrease |
|`--carry-chain-width`| Pipelining of add and subtract operations | Increase |
|`--fifo-merge-distance`| Distance between merged FIFOs with different enqueue locations | Increase |
|`--semaphore-delay`| Pipelining of thread count control signals | Decrease |
|`--return-stages`| Pipelining of return values from functions with multiple call sites | Decrease |
|`--rmw-memory-write-delay`| Pipelining of memory write data and control signals | Decrease |
|`--rmw-hardened-write-delay`| Pipelining of memory write data and control signals when hardened bypass support is available | Decrease |
|`--rmw-memory-read-delay`| Pipelining of memory read data | Decrease |
|`--rmw-memory-favor-area`| Pipelining of `[[schedule()]]` blocks that access memories | Enable |
|`--io-register-delay`| Estimated routing delay to/from memory/DSP registers | Decrease |
|`--reset-cycles`| Pipelining of reset signal | Decrease |
|`--register-ratio`| Logic depth | Increase |
|`--stallable-pipelines`| Decreases FIFO depth with fine-grain control signals | Enable |

## Trade logic for DSPs
The following compiler switch can enable trading ALMs for DSPs. This is only implemented for Stratix10 currently.

|Switch |Description  | Adjustment to lower logic area|
|--- | --- | ---|
|`--fifo-to-dsp`| Convert certain FIFOs to use DSPs | Increase |

## Trade registers for memories
The following compiler switch can enable trading registers for memories.
|Switch |Description  | Adjustment to lower register area|
|--- | --- | ---|
|`--mem-to-reg`| Upper threshold to move data from registers to memory | Increase |

## Trade registers for logic
The `--carry-select` compiler switch can decrease pipeline depth (and hence register count) by
implementing addition, subtraction, and comparison operations with replicated,
speculative hardware.  This switch is typically not useful on FPGAs which have dedicated carry hardware.

## When to stop
The following heuristics can be useful in determining how much potential there is to be gained
by further area optimization.  A well optimized Kanagawa design has the following properties:

* Area consumed by FIFOs should be less than 10% of total area
* Ratio of logic (LUT on FPGA, combinational area on ASIC) to registers should be >= 1
* Have an explanation for each non-inline function in the resource report,
why were these functions not inlined? See [Use inline functions](#use-inline-functions).
* Have an explanation for each basic block in the resource report,
what caused each basic block to be instantiated? See [Minimize Basic Block Count](#minimize-basic-block-count).

## Thread Rate
By default, Kanagawa functions have a peak throughput of one thread
entering/exiting the function per clock cycle.  This peak throughput is called the
thread rate (also known as initiation interval).  In some cases, this thread
rate is faster than necessary.  Examples include:

* There is an upper bound on the rate of arrival of new inputs
* On average, each thread must loop through a common portion of the design `N`
  times.  If the common portion has a thread rate of 1, then the remainder of
  the design is will run no faster than 1 thread every `N` clock cycles.

Area savings can be achieved by lowering the maximum thread rate of a portion of
a design.  When a thread rate is specified, the Kanagawa compiler enforces the
thread rate and may take advantage of the thread rate to reduce resource usage.

The thread rate of a specific function can be specified with the `[[thread_rate]]`
attribute:

```
class Foo
{
    // Peak throughput of 1 thread every 2 clock cycles
    [[thread_rate(2)]] uint32 F(uint32 x)
    {
        return x + 3;
    }
}
```

The thread rate of a function has no effect on the correctness of a function, it
is only a hint that allows area to be saved at the expense of throughput.
Scheduling constraints specified with `[[schedule(N)]]` and `atomic` should
_not_ be modified when adjusting the thread rate of a function.
`[[schedule(N)]]` means that no more than `N` threads can be present a block at
any particular time.  This is orthogonal to the rate at which threads enter the
associated function.

# Frequency

## Reset
The `--reset-cycles=N` command line parameter determines the number of cycles
used to pipeline the reset signal throughout the chip.  This process is divided
into two structures.  First a fan-out tree is used to replicate the reset
signal.  The `--reset-fan-out-cycles=M` parameter determines the number of clock
cycles (`M`) used by the fan-out tree.  After that, each replica is pipelined via a
chain (of depth `N-M`) of registers to its final destination.

A higher value of `--reset-fan-out-cycles=M` reduces the maximum fan-out of any
one reset register but adds artificial placement constraints by coupling replicas
that share a common ancestor in the fan-out tree.

## Allow wide operations to be decomposed
The compiler will internally decompose wide operations into multiple narrower operations
scheduled over multiple pipeline stages.  The following table describes compiler switches
that control this decomposition.

|Switch |Description |
|--- | --- |
|`--carry-chain-width`| Width of an add/subtraction operation considered to be 1 level of logic |
|`--max-mux-sources`| Maximum number of multiplexers inputs |
|`--max-mul-src-width`| Maximum source operand width (in bits) of a non-DSP multiplier |

## Decompose atomic blocks
In some cases, and atomic block can be decomposed into multiple sequential
atomic blocks.  This can increase frequency by removing some logic from the
critical path.

Original:

```
inline void F(uint32 count)
{
    atomic
    {
        static uint2 _i = 0;
        static uint32[4] _ary = {};

        // Determine array index
        uint2 index = _i;
        _i++;

        // Add `count` to one array element
        auto value = _ary[index];
        value += count;
        _ary[index] = value;
    }
}
```

Optimized:

```
inline void F(uint32 count)
{
    uint2 index; // local variable

    atomic
    {
        static uint2 _i = 0;

        // Determine array index
        index = _i;

        _i++;
    }

    atomic
    {
        static uint32[4] _ary = {};

        // Add `count` to one array element
        auto value = _ary[index];
        value += count;
        _ary[index] = value;
    }
}
```

### Break up atomic adds (or other wide operations)
An atomic add can be broken up into two (or more) atomic adds to improve frequency.

Original:

```
uint32 value;
atomic
{
    static uint32 counter;
    counter += value;
}
```

Optimized:

```
uint32 value;
uint16 value_lo = cast<uint16>(value); // Low 16 bits
uint16 value_hi = value >> 16; // High 16 bits

bool carry;
atomic
{
    static uint16 counter_lo;
    // Determine if add will overflow
    carry = (counter_lo + value_lo > 0xffff);
    // Add low 16 bits
    counter_lo += value_lo;
}

uint16 value_hi_with_carry = value_hi + cast<uint1>(carry);
atomic
{
    static uint16 counter_hi;
    counter_hi += value_hi_with_carry;
}
```

## Move logic outside of atomic
Refactoring logic outside of an atomic can improve its frequency.

Original:

```
uint32 x;
uint32 y;

atomic
{
    static uint32 counter;
    if (x == y)
    {
        counter += y;
    }
}
```

Optimized:

```
uint32 x;
uint32 y;

bool cond = (x == y);
uint32 increment = cond ? y : 0;
atomic
{
    static uint32 counter;
    counter += increment;
}
```

## Minimize logic depth within atomic
Maximum clock frequency may be limited by the depth of a chain of dependent computations inside an atomic block.
Minimize the depth of such chains of computations.

A common pattern to avoid an addition followed by a comparison is to pre-compute a value, like `count-1` in the following example:

Original:

```
inline void F(uint32 count)
{
    uint32 z = 4;

    atomic
    {
        uint32 _i = 0;

        _i++;

        // This comparison runs after the addition has completed
        if (_i == count)
        {
            z++;
        }
    }
}
```

Optimized:

```
inline void F(uint32 count)
{
    uint32 z = 4;

    uint32 count_minus_one = count - 1;

    atomic
    {
        uint32 _i = 0;

        // This comparison runs in parallel with the addition
        if (_i == count_minus_one)
        {
            z++;
        }

        _i++;
    }
}
```

## Parallel state updates
A common pattern in Kanagawa code is an atomic block that updates shared
variables based on the current values of the shared variables and the values of
thread local variables.

The examples in this section use the `State` type and `update_state` function
defined here:

```
module parallel_state_update_example
{ State
 , StateCount
 , update_state
}

// 4 possible state values
enum State : uint2
{
    Idle,
    Preparing,
    Executing,
    Finalizing
}

//| Total number of possible states.
const auto StateCount = 4;

//| A function that computes a new state given
// a current state and input data
inline State update_state(State curr_state, uint8 input_data)
{
    State new_state = curr_state;

    switch (curr_state)
    {
    case State::Idle:
        if (input_data > 0)
        {
            new_state = State::Preparing;
        }
        break;

    case State::Preparing:
        new_state = State::Executing;
        break;

    case State::Executing:
        if (input_data == 0)
        {
            new_state = State::Finalizing;
        }
        break;

    case State::Finalizing:
        new_state = State::Idle;
        break;
    }

    return new_state;
}
```

The code to handle one state update per thread is straightforward:

```
import parallel_state_update_example

// Calls update_state once per byte
inline void process_one_byte(uint8 input_data)
{
    atomic
    {
        static State current_state = {};

        current_state = update_state(current_state, input_data);
    }
}
```

If the throughput of this code is insufficient for a task, then it can be
parallelized such that each thread processes multiple bytes of input data per
cycle.  A naive way to parallelize the code is to serialize the calls
to `update_state` in the `atomic` block.  For example:

```
import parallel_state_update_example

inline State process_eight_bytes(uint8[8] input_data_array)
{
    State result;

    atomic
    {
        static State current_state = State::Idle;

        static for (const auto i : 8)
        {
            current_state = update_state(
                current_state,
                input_data_array[i]);
        }

        result = current_state;
    }

    return result;
}
```

The data dependency between the loop iterations would negatively impact the
frequency of the generated circuit because all loop iterations must execute
serially in one clock cycle.

A technique to achieve high throughput and high frequency is to refactor the
algorithm to perform most of the work outside of the `atomic` block.  This can
be accomplished by pre-computing the final state for each possible starting
state into a compact table.  Then the only operation left to be performed
atomically is a table lookup.

The following example uses functions from the `control.fsm` and `sync.atomic`
modules to implement this pattern.

```
import parallel_state_update_example
import control.fsm
import sync.atomic

inline State process_eight_bytes(uint8[8] input_data_array)
{
    // Precompute the new state for all possible current states.
    auto possible_new_states = speculate_updates(
        [input_data_array](index_t<StateCount> state_index)
        {
            auto state = cast<State>(state_index);

            static for (const auto i : 8)
            {
                state = update_state(
                    state,
                    input_data_array[i]);
            }

            return state;
        },
        {});

    //| Atomically select an update from `possible_new_states` and apply it.
    return second(atomically(
        apply_update(
            [](State[StateCount] updates, State prev)
            { return updates[prev]; },
            [](State update, State prev)
            { return update; },
            possible_new_states)));
}
```

# Power

## Clock Gating
Kanagawa supports multiple levels of clock gating to ensure that idle hardware
does not consume dynamic power (for ASIC targets only).

### Clock Gating Level
The clock gating level is set with the `--clock-gating=N` compiler option.
`--clock-gating=0` is the default setting, and disables all clock gating.
`--clock-gating=1` gates clocks feeding idle pipeline stages.
This reduces dynamic power for periods of time when portions of the design process
threads with a throughput of less than 1 thread per clock cycle.
 `--clock-gating=2` additionally gates clocks feeding pipeline registers that hold
 values that will not be needed because of control flow.

For example, if `--clock-gating=2` is specified when the following code is compiled,
then the hardware associated with the code inside of the `if` statement will not
consume dynamic power when `b` is false.

```
bool b;
uint32 x = 4;

if (b)
{
    x = x * 12345;
}
```

`--clock-gating=2` can increase area of a design because:
* Clock gates consume area
* Fine-grained clock gating makes some optimizations in the Kanagawa compiler less effective

### Clock Gating Threshold
Clock gates themselves consume dynamic power.  Clock gates only reduce overall power when the power
consumed by a clock gate is less than the power saved in the gated registers.  The `--clock-gating-threshold=N`
compiler option controls the tradeoff point.  A clock gate is only instantiated if the total number
of register bits that would be gated is above the threshold.

### Clock Gating Report
The Kanagawa compiler produces a report that indicates which registers are subject to a
clock gate based on control flow in the source code.  If this report shows a high percentage
of un-gated registers then there may be opportunity to reduce dynamic power by adding control flow
to the source code (and enabling `--clock-gating=2`).

