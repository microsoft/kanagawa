#pragma once
#define sim_assert(x) if (!(x)) {asm volatile ("ebreak");}

