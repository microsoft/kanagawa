#include "../test.h"

int main()
{
    int* invalid = 0;

    ENABLE_TRAP_CAPTURE(TRAP_ACCESS_FAULT)

    *invalid = 0; // Should generate an AccessFault

    ASSERT_TRAP_OCCURRED(TRAP_ACCESS_FAULT)

    CLEAR_TRAP_CAPTURE()

    return 0;
}
