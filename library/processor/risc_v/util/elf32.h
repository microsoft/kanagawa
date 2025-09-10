// Created based on ELF specification https://refspecs.linuxfoundation.org/elf/elf.pdf
//

#ifndef __ELF32__
#define __ELF32__

#include <vector>
#include <cstdint>
#include <stdexcept>

using Elf32_Addr  = uint32_t;
using Elf32_Half  = uint16_t;
using Elf32_Off   = uint32_t;
using Elf32_Sword = int32_t;
using Elf32_Word  = uint32_t;

#define EI_NIDENT 16
typedef struct {
    unsigned char e_ident[EI_NIDENT];
    Elf32_Half e_type;
    Elf32_Half e_machine;
    Elf32_Word e_version;
    Elf32_Addr e_entry;
    Elf32_Off e_phoff;
    Elf32_Off e_shoff;
    Elf32_Word e_flags;
    Elf32_Half e_ehsize;
    Elf32_Half e_phentsize;
    Elf32_Half e_phnum;
    Elf32_Half e_shentsize;
    Elf32_Half e_shnum;
    Elf32_Half e_shstrndx;
} Elf32_Ehdr;

typedef struct {
    Elf32_Word p_type;
    Elf32_Off p_offset;
    Elf32_Addr p_vaddr;
    Elf32_Addr p_paddr;
    Elf32_Word p_filesz;
    Elf32_Word p_memsz;
    Elf32_Word p_flags;
    Elf32_Word p_align;
} Elf32_Phdr;

#define PT_LOAD 0x1

#define PF_X	0x1	// Execute
#define PF_W	0x2	// Write
#define PF_R	0x4 // Read

template <typename Transfer>
uint32_t LoadElf(Transfer transfer_imem, size_t imem_max_batch_size, Transfer transfer_dmem, size_t dmem_max_batch_size, const void* elf, size_t size)
{
    auto ehdr = static_cast<const Elf32_Ehdr*>(elf);
    auto phdr = static_cast<const Elf32_Phdr*>(static_cast<const void*>(ehdr + 1));
    auto data = static_cast<const uint32_t*>(elf);

    if (size < sizeof(*ehdr) || size < sizeof(*ehdr) + ehdr->e_phnum * sizeof(*phdr))
    {
        throw std::runtime_error("Invalid ELF file");
    }

    std::vector<uint32_t> zeros(std::max(imem_max_batch_size, dmem_max_batch_size), 0);

    for (int i = 0; i < ehdr->e_phnum; ++i)
    {
        if (phdr[i].p_type == PT_LOAD)
        {
            uint32_t addr = phdr[i].p_vaddr;
            size_t msz = phdr[i].p_memsz / sizeof(int32_t);
            size_t filesz = phdr[i].p_filesz / sizeof(int32_t);

            bool executable = phdr[i].p_flags & PF_X;

            auto max_batch_size = executable ? imem_max_batch_size : dmem_max_batch_size;
            auto transfer = executable ? transfer_imem : transfer_dmem;

            size_t word_index = 0;

            while (word_index < filesz)
            {
                auto batch_size = std::min(filesz - word_index, max_batch_size);

                if (size < phdr[i].p_offset + (batch_size + word_index) * sizeof(int32_t))
                {
                    throw std::runtime_error("Invalid segment p_offset/p_filesz in ELF file");
                }

                transfer(
                    batch_size,
                    phdr[i].p_vaddr + static_cast<uint32_t>(word_index * sizeof(int32_t)),
                    data + phdr[i].p_offset / sizeof(*data) + word_index,
                    batch_size * sizeof(*data));
                word_index += batch_size;
            }

            while (word_index < msz)
            {
                auto batch_size = std::min(msz - word_index, max_batch_size);

                transfer(
                    batch_size,
                    phdr[i].p_vaddr + static_cast<uint32_t>(word_index * sizeof(int32_t)),
                    zeros.data(),
                    batch_size * sizeof(zeros[0]));
                word_index += batch_size;
            }
        }
    }

    return ehdr->e_entry;
}

#endif // __ELF32__

