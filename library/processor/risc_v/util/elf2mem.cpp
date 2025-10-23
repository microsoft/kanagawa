#include "elf32.h"

#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <functional>
#include <iomanip>

using namespace std::placeholders;

template <typename T>
std::vector<T> LoadFile(const std::string& fileName)
{
    std::vector<T> buf;
    std::ifstream file(fileName, std::ios::binary);

    if (!file.good())
    {
        throw std::runtime_error("Failed to open " + fileName);
    }

    file.seekg(0, std::ios::end);
    buf.resize(file.tellg() / sizeof(T));
    file.seekg(0, std::ios::beg);
    file.read(static_cast<char*>(static_cast<void*>(buf.data())), buf.size() * sizeof(T));

    return std::move(buf);
}

void WriteMem(std::vector<uint32_t>& mem, uint32_t base_addr, size_t unused, uint32_t addr, const uint32_t* data, size_t size)
{
    mem.resize((addr - base_addr + size) / sizeof(*data));

    std::copy(data, data + size / sizeof(*data), mem.begin() + (addr - base_addr) / sizeof(*data));
}

void WriteFile(const std::vector<uint32_t>& mem, std::string fileName)
{
    std::ofstream file(fileName.append(".mem"));

    if (!file.good())
    {
        throw std::runtime_error("Failed to open " + fileName);
    }

    for (const auto x : mem)
    {
        file << std::hex << std::setfill('0') << std::setw(8) << x << std::endl;
    }
}

int main(int argc, const char** argv)
{
    if (argc != 4)
    {
        std::cout << "Usage: elf2mem <elf_file> <imem_base_addr> <dmem_base_addr>" << std::endl;
        return 1;
    }

    try
    {
        const auto elf = LoadFile<int32_t>(argv[1]);
        const uint32_t imemBaseAddr = std::stoul(argv[2]);
        const uint32_t dmemBaseAddr = std::stoul(argv[3]);

        std::vector<uint32_t> imem, dmem;

        auto entry_point = LoadElf(
            std::bind(WriteMem, std::ref(imem), imemBaseAddr, _1, _2, _3, _4),
            0x80000,
            std::bind(WriteMem, std::ref(dmem), dmemBaseAddr, _1, _2, _3, _4),
            0x80000,
            elf.data(), elf.size() * sizeof(int32_t));

        std::string baseName = argv[1];

        WriteFile(imem, baseName + ".imem");
        WriteFile(dmem, baseName + ".dmem");

        std::cout << entry_point << std::endl;
    }
    catch(const std::exception& e)
    {
        std::cout << e.what();
        return 1;
    }

    return 0;
}
