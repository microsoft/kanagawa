# Kanagawa: Wavefront Threading for Hardware Design

Kanagawa is a high-level, imperative programming language for hardware design.
It introduces **Wavefront Threading**, a new execution model that makes concurrency explicit, enabling efficient and predictable parallel hardware.

Unlike traditional HLS tools, which often require pragmas and heuristics to guide the compiler, Kanagawa allows you to directly express concurrency, synchronization, and scheduling in code. This combines the productivity of software-like programming with the performance and efficiency of hand-written RTL.

## Background

Kanagawa addresses a longstanding challenge in digital design: bridging the gap between high-level programming and efficient hardware implementation. Conventional HDLs like Verilog and VHDL offer control but little abstraction. Existing HLS tools promise productivity but often fall short in predictable performance. Kanagawa takes a different approach.

### Key Features
- **Wavefront Threading**: Threads execute in a deterministic order, eliminating much of the synchronization burden.
- **Wavefront Consistency**: A memory model aligned with synchronous circuits for clarity and correctness.
- **Composable Abstractions**: Functions, lambdas, closures, and strong static typing provide zero-cost abstractions.
- **Reusable Libraries**: Includes generic data structures, algorithms, and IP blocks (e.g., RISC-V cores, floating point).

### Productivity and Efficiency
Kanagawa designs are highly competitive with hand-written RTL in area, performance, and power, while requiring far less code. Programmers can quickly explore design tradeoffs—static scheduling, dynamic scheduling, speculative execution—without rewriting entire designs.

### Real-World Impact
Kanagawa has been used for accelerators in Microsoft Azure, cryptographic research prototypes, and even full RISC-V cores. In benchmarks, Kanagawa implementations have matched or surpassed SystemVerilog designs, often with less than half the code.

### Open Source
This repository provides the Kanagawa language, compiler toolchain, and libraries. We welcome contributions from hardware designers, language researchers, and anyone curious about pushing hardware programming forward.

## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit [Contributor License Agreements](https://cla.opensource.microsoft.com).

When you submit a pull request, a CLA bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., status check, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.

## Trademarks

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft
trademarks or logos is subject to and must follow
[Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/legal/intellectualproperty/trademarks/usage/general).
Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship.
Any use of third-party trademarks or logos are subject to those third-party's policies.

# Building

Please see the file [BUILDING.md](BUILDING.md)
