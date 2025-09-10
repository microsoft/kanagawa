#include <core_portme.h>

int main()
{
    unsigned cycleh_start, cycle_start;

    ee_printf("TEST 1 2 3\n");
    ee_printf("TEST 1 2 3\n");
    ee_printf("TEST 1 2 3\n");
    ee_printf("TEST 1 2 3\n");

    for(;;)
    {
        unsigned tmp;
        asm volatile ("rdcycleh %0" : "=r"(cycleh_start));
        for (int i = 0; i < 32; ++i)
        {
            asm volatile ("rdcycle %0" : "=r"(cycle_start));
        }
        asm volatile ("rdcycleh %0" : "=r"(tmp));

        if (cycleh_start == tmp) break;
    }

    unsigned cycleh_end, cycle_end;

    do
    {
        asm volatile ("rdcycle %0" : "=r"(cycle_end));
    }
    while (cycle_end > cycle_start);

    asm volatile ("rdcycleh %0" : "=r"(cycleh_end));

    if (cycleh_end - cycleh_start == 1)
    {
        ee_printf("PASS\n");
    }

    return 0;
}
