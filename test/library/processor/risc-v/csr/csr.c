#include "../test.h"

unsigned g;

void test_instret()
{
    unsigned instret_start;
    unsigned instret_end;
    volatile unsigned* x = &g;

    *x = 12;
    *x += 1;

    asm volatile ("rdinstret %0" : "=r"(instret_start));

    if (*x != 13)
    {
        *x += 1;
    }

    asm volatile ("rdinstret %0" : "=r"(instret_end));

    sim_assert(instret_end - instret_start == 6);
}

void test_time()
{
    unsigned time_start;
    unsigned time_end;

    asm volatile ("rdtime %0" : "=r"(time_start));
    asm volatile ("rdtime %0" : "=r"(time_end));

    sim_assert(time_start != time_end);

    unsigned timeh_start;
    unsigned timeh_end;

    asm volatile ("rdtimeh %0" : "=r"(timeh_start));
    asm volatile ("rdtime %0"  : "=r"(time_start));
    asm volatile ("rdtime %0"  : "=r"(time_end));
    asm volatile ("rdtimeh %0" : "=r"(timeh_end));

    sim_assert(time_end > time_start || timeh_end > timeh_start);
}

void test_cycle()
{
    unsigned cycle_start;
    unsigned cycle_end;

    asm volatile ("rdcycle %0" : "=r"(cycle_start));
    asm volatile ("rdcycle %0" : "=r"(cycle_end));

    sim_assert(cycle_start != cycle_end);

    unsigned cycleh_start;
    unsigned cycleh_end;

    asm volatile ("rdcycleh %0" : "=r"(cycleh_start));
    asm volatile ("rdcycle %0"  : "=r"(cycle_start));
    asm volatile ("rdcycle %0"  : "=r"(cycle_end));
    asm volatile ("rdcycleh %0" : "=r"(cycleh_end));

    sim_assert(cycle_end > cycle_start || cycleh_end > cycleh_start);
}

void test_mhartid()
{
    volatile unsigned* const phid = (unsigned*)0x80220004;

    unsigned hid = 0xdeadbeef;

    asm volatile ("csrr %[rd], mhartid"
            : [rd] "=r" (hid));

    sim_assert(hid == *phid);
}

void test_csrr()
{
    unsigned value = 0xdeadbeef;

    asm volatile ("csrr %[rd], 0xC03"
            : [rd] "=r" (value));

    asm volatile ("csrr %[rd], 0xC11"
            : [rd] "=r" (value));

    // Set trap mask so we can determine if an IllegalInstruction trap occurs
    ENABLE_TRAP_CAPTURE(TRAP_ILLEGAL_INSTRUCTION)

    // This next instruction should generate an illegal instruction trap (Trap::IllegalInstruction)
    // because 0xC21 is not a valid CSR.
    asm volatile ("csrr %[rd], 0xC21"
            : [rd] "=r" (value));

    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION);

    CLEAR_TRAP_CAPTURE()
}

int main()
{
    test_instret();
    test_time();
    test_cycle();
    test_mhartid();
    test_csrr();

    return 0;
}
