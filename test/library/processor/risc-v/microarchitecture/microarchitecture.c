int main()
{
    register int hid;

    asm volatile ("csrr %[rd], mhartid" : [rd] "=r" (hid));

    if (hid == 0)
    {
        unsigned g;
        int instret_start, instret_end, time_start, time_end;
        volatile unsigned* x = &g;

        asm volatile ("csrr %0, time" : "=r"(time_start));
        asm volatile ("csrr %0, time" : "=r"(time_end));

        asm volatile ("csrr %0, instret" : "=r"(instret_start));
        asm volatile ("csrr %0, instret" : "=r"(instret_end));

        int harts = (time_end - time_start) / (instret_end - instret_start);

        int bubbles, bubbles_last;

        for (int i = 0; i < 5; ++i)
        {
            asm volatile ("csrr %0, time" : "=r"(time_start));
            asm volatile ("csrr %0, instret" : "=r"(instret_start));

            if (*x != 4)
            {
                ++*x;
            }
            else
            {
                --*x;
            }

            asm volatile ("csrr %0, time" : "=r"(time_end));
            asm volatile ("csrr %0, instret" : "=r"(instret_end));

            bubbles_last = bubbles;
            bubbles = (time_end - time_start) / harts - (instret_end - instret_start);
        }

        int* pPenalty = (int*)0x80200000;
        int* pHarts   = (int*)0x80200004;
       
        *pPenalty = bubbles - bubbles_last;
        *pHarts = harts;

        asm volatile ("ecall");
    }
    else
    {
        for(;;);
    }
}
