#include "../test.h"

int main()
{
    void (*invalid)() = 0;

    // Tell trap handler that we expected this trap. For this particular
    // trap it will just exit the simulation.
    ENABLE_TRAP_CAPTURE(TRAP_INVALID_INSTRUCTION_ADDRESS)

    invalid();

    // Should never get here
    sim_assert(0);

    return 0;
}
