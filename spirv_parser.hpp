#include <spirv/unified1/spirv.h>

#include <win32_helper.hpp>

namespace spirv_parser {

using word = uint32_t;
using id = word;

template<bool HasResult=false, bool HasInstructionType=false, uint32_t OperandsSize=0>
struct instruction {
    word word_count;
    word opcode;
    [[no_unique_address]]
    cpp_helper::valid_if_t<HasResult, id> result;
    [[no_unique_address]]
    cpp_helper::valid_if_t<HasInstructionType, word> type;
    [[no_unique_address]]
    cpp_helper::valid_if_t<OperandsSize!=0, std::array<word,OperandsSize>> operands;
};

static_assert(
    std::is_same_v<decltype(instruction<>{}.result), cpp_helper::empty_type> &&
    std::is_same_v<decltype(instruction<true>{}.result), id>
);

auto open_spirv_file(std::filesystem::path path) {
    auto file_mapping = win32_helper::map_file(path);
    auto spirv_code = std::span{reinterpret_cast<const uint32_t*>(file_mapping.data()), file_mapping.size()/sizeof(uint32_t)};
    return std::pair{std::move(file_mapping), spirv_code};
}

}