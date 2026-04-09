# Kanagawa Compiler Design Reference

This document is a source-backed design reference for the open-source Kanagawa compiler as it exists in this repository. It is meant to help both human developers and Copilot work in the codebase without relying on stale internal documentation or inferred behavior.

The emphasis here is on verified structure:

1. what stages exist in the shipped compiler,
2. which concrete data structures carry information between those stages,
3. which subsystems own each transformation,
4. where the corresponding code lives.

Each section ends with direct source anchors to the relevant files and entry points.

## 1. Repository-Level Architecture

The compiler is split across three major implementation layers.

| Layer | Primary location | Responsibility |
| --- | --- | --- |
| Frontend parsing and desugaring | [compiler/hs](../compiler/hs) | Parse source files, merge modules, run frontend/desugaring/type-inference passes, and translate the typed Haskell AST into the C++ ParseTree API. |
| Middle end and scheduling | [compiler/cpp](../compiler/cpp) | Own the ParseTree, type checking, object/function-instance enumeration, IR generation, optimization, scheduling, pipelining, and report generation. |
| Hardware backend | [compiler/cpp/verilog.cpp](../compiler/cpp/verilog.cpp), [compiler/cpp/circt_util.h](../compiler/cpp/circt_util.h), [runtime/rtl](../runtime/rtl) | Lower the compiler IR into CIRCT/MLIR-backed SystemVerilog plus runtime support modules and wrappers. |

The frontend and middle end are not separated by a serialized file format. The Haskell executable calls into the C++ backend through the ParseTree FFI and then invokes the C++ `Codegen` entry point directly.

Source anchors: [compiler/hs/app/Main.hs](../compiler/hs/app/Main.hs) (`main`, `handle`), [compiler/hs/app/ParseTree.hs](../compiler/hs/app/ParseTree.hs) (`compile`), [compiler/cpp/parse_tree.h](../compiler/cpp/parse_tree.h) (`InitCompiler`, `Codegen`, `Parse*` declarations), [compiler/cpp/kanagawa.cpp](../compiler/cpp/kanagawa.cpp) (`Codegen`).

## 2. End-to-End Compilation Flow

At a high level, the shipped compiler performs the following steps:

1. The Haskell executable parses the requested source files with `parseProgram`.
2. The parsed modules are merged and passed through the Haskell frontend pipeline via `frontend`.
3. The typed Haskell AST is recursively translated into C++ ParseTree nodes by `ParseTree.compile`.
4. `InitCompiler` creates and configures the C++ compiler state.
5. `Codegen` performs C++ type checking, declaration reordering, device configuration extraction, device-capability validation, and compiled-module enumeration.
6. For each `CompiledModule`, the compiler resets per-module state, externalizes classes, registers objects, enumerates reachable function instances, and builds a `Program` IR.
7. The generated IR is optimized, scheduled, pipelined, validated, and prepared for backend emission.
8. Non-default compiled modules are emitted as SystemVerilog, package files, headers, reports, symbol/debug maps, and optional CIRCT IR dumps.

One important detail is that `Codegen` always creates a leading “default” compiled module. That pass compiles shared/global extern/export state but intentionally skips output emission; output files are generated only for non-default compiled modules.

Source anchors: [compiler/hs/app/Main.hs](../compiler/hs/app/Main.hs) (`handle`), [compiler/hs/lib/Language/Kanagawa/Frontend.hs](../compiler/hs/lib/Language/Kanagawa/Frontend.hs) (`frontend`, `desugarPasses`), [compiler/hs/app/ParseTree.hs](../compiler/hs/app/ParseTree.hs) (`compile`, `parseWithLoc`), [compiler/cpp/kanagawa.cpp](../compiler/cpp/kanagawa.cpp) (`Codegen`), [compiler/cpp/compiler.cpp](../compiler/cpp/compiler.cpp) (`EnumerateCompiledModules`, `Reset`), [compiler/cpp/verilog.cpp](../compiler/cpp/verilog.cpp) (`CompileVerilog`, `VerilogCompiler::Compile`).

## 3. Haskell Frontend

### 3.1 Entry point and mode selection

The executable entry point is `main` in the Haskell app. In compile mode, `handle` parses all requested files, merges the resulting AST fragments, runs the frontend pipeline, reports warnings, and finally calls `compile` from the ParseTree bridge.

This means the Haskell side owns user-facing parsing, pretty-printing, warnings, frontend pass configuration, and the final handoff into the C++ compiler.

Source anchors: [compiler/hs/app/Main.hs](../compiler/hs/app/Main.hs) (`main`, `handle`, `getParseOptions`).

### 3.2 Frontend pass pipeline

The authoritative frontend pass list is `desugarPasses` in `Language.Kanagawa.Frontend`. The pass pipeline is not described abstractly in a separate config file; it is encoded as an ordered list of transformations and validations in Haskell.

The current pass sequence includes, among other steps:

- program validation and desugaring,
- interpolated-string lowering,
- `auto` deduction,
- post-desugar cleanup,
- template removal and unresolved-template handling,
- extern/export validation,
- undefined-symbol detection,
- template instantiation,
- extern-class trimming,
- type inference,
- intrinsic lowering,
- template argument deduction,
- enum reification and `this` capture,
- higher-order-function lowering,
- final validation of typed literals, interpolated strings, non-inline functions, local member functions, and redundant name scope.

The key design point is that the Haskell frontend is responsible for producing a typed, lowered AST before any ParseTree nodes are created on the C++ side.

Source anchors: [compiler/hs/lib/Language/Kanagawa/Frontend.hs](../compiler/hs/lib/Language/Kanagawa/Frontend.hs) (`frontend`, `desugarPasses`, `postDesugar`, `captureThis`, `reifyEnums`, `undefinedSymbols`).

### 3.3 ParseTree translation boundary

`ParseTree.compile` is the bridge from the typed Haskell AST into the C++ compiler. It does four important things:

1. marshals CLI/backend options into the FFI layer with `withOptions` and `initCompiler`,
2. walks the typed AST recursively with `cataM parseWithLoc`,
3. sets source locations on every translated node via `setLocation` or `unknownLocation`,
4. calls the C++ backend entry point `codegen` after the ParseTree has been built.

`parseWithLoc` also explicitly annotates integer and enum nodes with C++ type information through `setNodeType`, so the C++ side receives both structural and type metadata.

Source anchors: [compiler/hs/app/ParseTree.hs](../compiler/hs/app/ParseTree.hs) (`compile`, `parseWithLoc`, `setSrcLocation`, `parse`, `parseIntegerType`).

## 4. ParseTree FFI Surface and Ownership Model

The C ABI exposed by the C++ side is declared in [compiler/cpp/parse_tree.h](../compiler/cpp/parse_tree.h). It defines:

- the cross-language `Location` struct,
- enums for operators, loop modes, memory kinds, attributes, and function modifiers,
- the large family of `Parse*` construction routines used by the Haskell translator,
- metadata hooks such as `SetLocation`, `SetLocation2`, `SetNodeType`, and `UnknownLocation`,
- compiler entry points such as `InitCompiler` and `Codegen`.

The C++ `Compiler` does not expose a single arena object in the public API. Instead it owns two cleanup lists: `_permanentParseTreeCleanupList` for frontend-created nodes reused across compiled modules, and `_temporaryParseTreeCleanupList` for middle-end nodes rebuilt per module. `Reset` clears temporary parse-tree state between compiled-module passes.

This distinction matters because one invocation of `Codegen` may compile multiple `CompiledModule` variants from the same parsed program.

Source anchors: [compiler/cpp/parse_tree.h](../compiler/cpp/parse_tree.h), [compiler/cpp/compiler.h](../compiler/cpp/compiler.h) (`Compiler`, `_permanentParseTreeCleanupList`, `_temporaryParseTreeCleanupList`), [compiler/cpp/compiler.cpp](../compiler/cpp/compiler.cpp) (`Reset`).

## 5. C++ Compiler Orchestration

The top-level C++ entry point is `Codegen` in [compiler/cpp/kanagawa.cpp](../compiler/cpp/kanagawa.cpp). The function performs the following orchestration in order:

1. initialize the compiler and global state,
2. run `TypeCheck`,
3. run `CheckTypeNames`,
4. reorder declarations with `ReorderDeclarations`,
5. extract and validate device configuration,
6. enumerate the set of compiled modules,
7. mark permanent nodes,
8. for each module: `Reset`, `ExternalizeClasses`, `RegisterObjects`, `EnumerateFunctionInstances`, `GenerateIR`, and optionally backend emission,
9. for emitted modules, write Verilog/package/header/report outputs.

`Compiler::GenerateIR` is the main transition point from ParseTree to the middle-end IR. Inside that function the compiler:

- allocates registers for all parse-tree declarations,
- allocates the top-level export-class register set when compiling an export class,
- runs ParseTree IR generation through `_root->GenerateStatementIR(context)`,
- generates conditional returns,
- inserts stall logic when configured,
- links call sites to return blocks,
- links external-class callbacks,
- validates exports and externs,
- checks and rewrites atomic structures,
- removes frontend-only concepts and runs optimization/scheduling/pipelining.

Source anchors: [compiler/cpp/kanagawa.cpp](../compiler/cpp/kanagawa.cpp) (`Codegen`), [compiler/cpp/compiler.cpp](../compiler/cpp/compiler.cpp) (`GenerateIR`, `TypeCheck`, `CheckTypeNames`, `ExtractDeviceConfig`, `DeviceCapabilityCheck`).

## 6. Compiled Modules and Export-Class Compilation

Kanagawa can emit more than one backend module from one source program. That behavior is controlled by `CompiledModule` and `Compiler::EnumerateCompiledModules`.

`EnumerateCompiledModules` always creates a default pass first. It then scans `_parseTreeNodes` for export classes and, when appropriate, appends a non-default `CompiledModule` for each export class. The export-class pass carries:

- `_classNodeToCompile`,
- `_moduleName`,
- `_baseFileName`,
- `_placeholderObjectName`.

`Reset` then reconfigures ParseTree state for the selected module. For non-default passes it:

- removes export/external modifiers from ordinary non-external functions,
- marks the selected export class’s interface methods as exportable,
- creates extern callbacks for that class,
- marks non-selected export classes as external/export-class-interface and prunes their AST bodies for use-only mode.

The practical consequence is that export-class compilation is not a separate parser invocation; it is a controlled mutation/reuse of the existing ParseTree between module passes.

Source anchors: [compiler/cpp/compiler.h](../compiler/cpp/compiler.h) (`CompiledModule`), [compiler/cpp/compiler.cpp](../compiler/cpp/compiler.cpp) (`EnumerateCompiledModules`, `Reset`), [compiler/cpp/kanagawa.cpp](../compiler/cpp/kanagawa.cpp) (`Codegen`).

## 7. Type Checking and Semantic Validation

The main C++ semantic pass is `Compiler::TypeCheck`. It explicitly runs three passes over the same ParseTree and the same `TypeCheckContext`:

1. `TypeCheckPass::Functions`,
2. `TypeCheckPass::Globals`,
3. `TypeCheckPass::Default`.

The code comments state the reason directly: functions must be callable before definition, globals must be referenceable before declaration, and global declarations may reference functions such as callbacks.

After type checking, `CheckTypeNames` enforces that structs and unions do not collide with function names. Additional semantic validation is performed later in the pipeline, including device-config extraction, device-capability checks, export/extern checks, atomic validation/rewrite, inspection validation, and start-condition/global-write consistency checks.

Source anchors: [compiler/cpp/compiler.cpp](../compiler/cpp/compiler.cpp) (`TypeCheck`, `CheckTypeNames`, `ExtractDeviceConfig`, `DeviceCapabilityCheck`, `CheckInspection`, `CheckStartingGlobalWrites`).

## 8. Function-Instance Enumeration and Reachability

The ParseTree still contains source-level functions and objects. Before IR generation, the compiler enumerates concrete function instances.

The relevant logic lives in [compiler/cpp/enumerate_function_instances.cpp](../compiler/cpp/enumerate_function_instances.cpp). Verified points from that implementation:

- `FunctionInstance` is keyed by function node, object name, and instance index.
- `TraverseParseTree` walks the ParseTree, captures per-function symbol-table context, and records which methods belong to each class type.
- `RegisterObject` seeds the work list for methods that are always reachable, currently functions carrying `ParseTreeFunctionModifierExport` or `ParseTreeFunctionModifierReset`.
- `RegisterObject` also seeds methods callable from external classes and validates allowed callback modifiers.
- Non-inline instance numbering is allocated by `GetOrAllocateInstanceIndex`, keyed by object name plus parameter-reference context.

The rest of the compiler depends on this enumeration for reachability, call/return linking, and later removal of unreachable functions.

Source anchors: [compiler/cpp/enumerate_function_instances.cpp](../compiler/cpp/enumerate_function_instances.cpp) (`FunctionInstance`, `FunctionInstanceEnumerator`, `TraverseParseTree`, `RegisterObject`, `GetOrAllocateInstanceIndex`), [compiler/cpp/kanagawa.cpp](../compiler/cpp/kanagawa.cpp) (`Codegen` loop around `EnumerateFunctionInstances`).

## 9. Middle-End IR Data Structures

Kanagawa’s main compiler IR is defined in [compiler/cpp/ir.h](../compiler/cpp/ir.h). It is a register-based IR built around a global `Program` object rather than an SSA IR.

### 9.1 `Program`

`Program` is the top-level IR container. It owns:

- the generated module name and compiled-module mode flags,
- the global register table (`_registerTable`),
- entry points, callable entry points, and extern entry points,
- the list of non-inline `Function` objects,
- FIFO mergers, loop generators, context savers, and external module calls,
- exported types/typedefs,
- inspectable/code-coverage variables,
- extern class instances,
- placement/debug bookkeeping.

Source anchors: [compiler/cpp/ir.h](../compiler/cpp/ir.h) (`Program`).

### 9.2 `RegisterDescription`

`RegisterDescription` is the central description of storage in the IR. A register-table entry carries width, register kind, source/debug provenance, and a variant-specific descriptor for globals, globals views, locals, FIFOs, memories, and related backend state.

This table is the main namespace that operations refer to; later passes rename, compact, or map these registers but still work through register indices instead of SSA values.

Source anchors: [compiler/cpp/ir.h](../compiler/cpp/ir.h) (`RegisterDescription`, related helper types near the top of the file).

### 9.3 `Function`, `BasicBlock`, `Stage`, and `Operation`

The core executable hierarchy is:

- `Program` owns `Function` objects.
- Each `Function` owns a list of `BasicBlock` objects.
- Each `BasicBlock` owns a list of scheduled `Stage` objects plus a temporary unscheduled `_operations` list while the block is still being built.
- Each `Stage` owns an `OperationList` for one pipeline stage or one atomic sequence.
- Each `Operation` records its opcode, source operands, destination operands, flags/unions for opcode-specific payload, source locations, and renaming metadata.

`BasicBlock` also carries FIFO connectivity, live-in/live-out-related register lists, source location, object-name associations, loop/atomic flags, and transitive backpressure relationships. `Function` carries entry/exit blocks, return-FIFO index, thread-count bounds, semaphores, reachability, and synchronous-extern return-routing metadata.

Source anchors: [compiler/cpp/ir.h](../compiler/cpp/ir.h) (`Operation`, `Stage`, `BasicBlock`, `Function`).

## 10. IR Generation

`Compiler::GenerateIR` constructs a fresh `Program`, associates it with the current `CompiledModule`, and then drives IR generation from the ParseTree root.

The verified generation sequence is:

1. create `Program`, set `_moduleName`, `_isDefaultPass`, and `_exportClass`,
2. allocate registers for every ParseTree node that needs storage,
3. allocate top-level export-class storage when compiling an export class,
4. push IR/type/object/class context stacks,
5. call `_root->GenerateStatementIR(context)`,
6. post-process return nodes with `GenerateConditionalReturns`,
7. insert stall logic if configured,
8. resolve call-site return blocks,
9. link external-class callbacks,
10. run export/extern/atomic checks,
11. remove frontend-only concepts, optimize, schedule, and pipeline.

This is the stage where the compiler leaves AST-style semantics and starts operating on `Program`/`Function`/`BasicBlock`/`Operation` structures.

Source anchors: [compiler/cpp/compiler.cpp](../compiler/cpp/compiler.cpp) (`GenerateIR`).

## 11. Optimization Pipeline

The optimization driver is split across `OptimizeScheduleAndPipeline`, `Optimize`, and `EarlyOptimization`.

### 11.1 Pre-scheduling orchestration

`OptimizeScheduleAndPipeline` in [compiler/cpp/lower.cpp](../compiler/cpp/lower.cpp) performs the full post-IR-generation pipeline. Verified steps include:

- enable pre-pipeline CFG construction,
- uniquify debug-view labels,
- count synchronous-extern call sites,
- remove unreachable functions,
- mark FIFO writes that can be skipped,
- run `EarlyOptimization` when optimization is enabled,
- remove `ConditionalIgnore` operations inside atomic blocks,
- run register renaming on every basic block even when optimizations are otherwise disabled,
- validate IR,
- insert global views,
- run `Optimize`,
- update compare widths,
- remove newly unreachable functions to convergence,
- compact the register table,
- optionally serialize post-opt IR,
- remove empty atomic blocks when needed,
- always run `RemoveDeadJumps` after optimization.

Source anchors: [compiler/cpp/lower.cpp](../compiler/cpp/lower.cpp) (`OptimizeScheduleAndPipeline`).

### 11.2 Optimization phases

`optimize.h` defines the phase structure: `Start`, `PackLuts1`, `PackLuts2`, `LutCommonSub`, and `Synthesis`. `Optimize` runs phase-by-phase until no optimization reports forward progress.

Verified global optimizations are:

- `SubstituteLiterals`,
- `MemToArray` when compiling to Verilog and optimization is enabled,
- `RemoveUnusedGlobals`.

Verified function-level optimizations include, among others:

- `KillMoves`,
- constant propagation,
- select-index propagation,
- dead-jump removal,
- assert removal when configured,
- gather-to-move and add-to-or rewrites,
- string cleanup,
- LUT canonicalization and packing,
- common subexpression elimination,
- constant select-bit folding,
- literal narrowing,
- predicate simplification,
- conditional-ignore propagation,
- clock-gating computation,
- wide-op decomposition,
- algebraic identities,
- bit-level constant propagation,
- IR validation,
- dead-operation removal.

`EarlyOptimization` is a separate low-complexity, thread-pooled pass intended to reduce later optimization work before the heavier serial passes run.

Source anchors: [compiler/cpp/optimize.h](../compiler/cpp/optimize.h) (`OptimizationPhase`), [compiler/cpp/optimize.cpp](../compiler/cpp/optimize.cpp) (`Optimize`, `EarlyOptimization`).

## 12. Scheduling and Pipelining

Scheduling is implemented in [compiler/cpp/schedule.cpp](../compiler/cpp/schedule.cpp) around the `ConstraintScheduler`.

### 12.1 Constraint model

`ConstraintScheduler` models each schedulable item as a node with:

- a minimum legal pipeline stage,
- a current allowed stage range,
- a list of outgoing constraints,
- a pointer back to the underlying `Operation`.

The scheduler processes ready nodes whose incoming constraints have been satisfied, selects a pipeline stage within the current allowed range, and then tightens the allowable ranges of dependent nodes.

Source anchors: [compiler/cpp/schedule.cpp](../compiler/cpp/schedule.cpp) (`ConstraintScheduler`, `ConstraintScheduler::Range`, `ConstraintScheduler::Node`, `ConstraintScheduler::Schedule`).

### 12.2 Atomic-block fallback strategy

The scheduler has an explicit fallback strategy for atomic blocks, encoded by `AtomicBlockSchedulingPass`:

1. `Default`,
2. `IgnorePathLengthExceeded`,
3. `IgnoreRegisterRatio`,
4. `RemovePredication`,
5. `ReadLatencyOne`.

When constraints become impossible to satisfy, the scheduler walks failing atomic blocks from inner to outer, first increasing per-atomic-block register ratio up to the configured maximum, then advancing through the fallback modes above. If all strategies are exhausted, it reports `BasicBlockNotSchedulable`.

Source anchors: [compiler/cpp/schedule.cpp](../compiler/cpp/schedule.cpp) (`AtomicBlockSchedulingPass`, `ConstraintScheduler::Schedule`).

### 12.3 Dependency handling that affects generated hardware

The scheduler’s data-hazard constraint logic explicitly accounts for:

- path-length/register-ratio limits,
- registered outputs and registered inputs,
- global RAW/WAW hazards,
- BeginAtomic placement rules,
- routing slack for hardened blocks,
- special handling of predicated memory loads and predicated pure extern calls,
- hardened-register-to-hardened-register cases that require an extra pipeline stage.

Those are not just heuristics for a pretty schedule; they directly control where pipeline registers will be required in generated hardware.

Source anchors: [compiler/cpp/schedule.cpp](../compiler/cpp/schedule.cpp) (dependency-constraint construction in the main scheduling logic).

## 13. Backend and Output Artifacts

The Verilog backend entry point is `CompileVerilog`, which constructs a `VerilogCompiler` and calls `VerilogCompiler::Compile`.

Verified backend behavior from `VerilogCompiler::Compile` and the surrounding helpers:

- write the SystemVerilog package for exported types,
- create an MLIR/CIRCT module and design object,
- optionally declare debug-view modules,
- compile every non-extern basic block into backend modules,
- declare context-saver merger modules and ASIC memory-init LUT modules,
- declare the top-level core module and ports,
- instantiate reset control, memories, globals, inspectables, coverpoints, extern class instances, FIFOs, FIFO mergers, loop generators, context savers, and basic blocks,
- connect export and extern interfaces,
- optionally serialize CIRCT IR,
- round-trip verify the MLIR module,
- generate final SystemVerilog text from MLIR.

At the `Codegen` layer, emitted files include at least:

- `.sv`,
- `_types.sv`,
- `.cpp`,
- `.h`,
- `.tcl`,
- `HwConfig.mk`,
- symbol/debug-symbol CSVs,
- clock-gating and path-length reports,
- RTL map JSON,
- optional `.mlir` CIRCT assembly.

The CIRCT utility layer in [compiler/cpp/circt_util.h](../compiler/cpp/circt_util.h) defines the shared helpers for locations, type conversion, container-port access, lowering hooks, and dialect loading.

Source anchors: [compiler/cpp/verilog.h](../compiler/cpp/verilog.h) (`CompileVerilog`), [compiler/cpp/verilog.cpp](../compiler/cpp/verilog.cpp) (`CompileVerilog`, `VerilogCompiler::Compile`, `DeclareCore`, `DeclareCorePorts`, `InstantiateMemoriesCIRCT`, `InstantiateFifos`, `ConnectExportFifos`, `InstantiateBasicBlocks`), [compiler/cpp/circt_util.h](../compiler/cpp/circt_util.h).

## 14. Test and Validation Infrastructure

The repository uses two main validation paths.

### 14.1 End-to-end compiler and RTL tests

`build/cmake/test_helper.cmake` provides the reusable CMake entry points:

- `add_kanagawa` to run the Kanagawa compiler and generate RTL,
- `add_verilator` to compile SystemVerilog into a simulation executable with Verilator,
- `add_kanagawa_verilator_test` to compose both steps into a CTest.

The helper currently wires in a standard test option set including `--Wall`, `--Werror`, `--parse-docs`, `--frequency=250`, `--register-ratio=3`, `--backend=sv`, and the library import directory.

Library tests under [test/library](../test/library) use this infrastructure, and the default path is to generate `_test_runner_main.sv` and run it with a SystemVerilog testbench.

Source anchors: [build/cmake/test_helper.cmake](../build/cmake/test_helper.cmake) (`add_kanagawa`, `add_verilator`, `add_kanagawa_verilator_test`), [test/library/CMakeLists.txt](../test/library/CMakeLists.txt).

### 14.2 Internal compiler tests

The compiler also ships native internal tests. [test/compiler/CMakeLists.txt](../test/compiler/CMakeLists.txt) builds `compiler_internal_test`, registers it as the `compiler.internal` CTest, and provides convenience targets for running the compiler test set.

One verified example is `FixupLutTest` in [compiler/cpp/internal_tests.cpp](../compiler/cpp/internal_tests.cpp), which stress-checks LUT canonicalization/simplification by generating randomized LUT operations and comparing behavior before and after `FixupLutOps`.

Source anchors: [test/compiler/CMakeLists.txt](../test/compiler/CMakeLists.txt), [compiler/cpp/internal_tests.cpp](../compiler/cpp/internal_tests.cpp) (`FixupLutTest`).

## 15. Practical Guidance for Working in This Codebase

When changing compiler behavior, the most reliable subsystem boundaries are:

- Haskell frontend issues: start in [compiler/hs/app/Main.hs](../compiler/hs/app/Main.hs) and [compiler/hs/lib/Language/Kanagawa/Frontend.hs](../compiler/hs/lib/Language/Kanagawa/Frontend.hs).
- ParseTree translation issues: start in [compiler/hs/app/ParseTree.hs](../compiler/hs/app/ParseTree.hs) and [compiler/cpp/parse_tree.h](../compiler/cpp/parse_tree.h).
- ParseTree semantics, type checking, and module splitting: start in [compiler/cpp/compiler.cpp](../compiler/cpp/compiler.cpp) and [compiler/cpp/compiler.h](../compiler/cpp/compiler.h).
- Reachability and call-graph shape: start in [compiler/cpp/enumerate_function_instances.cpp](../compiler/cpp/enumerate_function_instances.cpp).
- IR shape and data transport: start in [compiler/cpp/ir.h](../compiler/cpp/ir.h).
- Optimization/scheduling/pipelining: start in [compiler/cpp/optimize.cpp](../compiler/cpp/optimize.cpp), [compiler/cpp/optimize.h](../compiler/cpp/optimize.h), [compiler/cpp/lower.cpp](../compiler/cpp/lower.cpp), and [compiler/cpp/schedule.cpp](../compiler/cpp/schedule.cpp).
- Verilog/CIRCT/backend issues: start in [compiler/cpp/verilog.cpp](../compiler/cpp/verilog.cpp), [compiler/cpp/verilog.h](../compiler/cpp/verilog.h), and [compiler/cpp/circt_util.h](../compiler/cpp/circt_util.h).
- End-to-end test wiring: start in [build/cmake/test_helper.cmake](../build/cmake/test_helper.cmake).

That division reflects the current shipped code, not an aspirational architecture. In particular, many important transformations still happen in the C++ middle end after ParseTree construction, and backend emission remains tightly coupled to the `Program`/register-table model.
