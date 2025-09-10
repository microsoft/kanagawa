#include "../riscv_test.h"

int main()
{
    typedef void(*pfn)();

    // invalid unaligned address
    pfn unaligned_address = (pfn)(IMEM_ORIGIN + 2);

    // Tell trap handler that we expected this trap. For this particular
    // trap it will just exit the simulation.
    ENABLE_TRAP_CAPTURE(TRAP_INVALID_INSTRUCTION_ADDRESS)

    (*unaligned_address)();

    // Should never get here
    sim_assert(0);

    return 0;
}
