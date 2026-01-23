#include <spirv/unified1/spirv.h>

#include <unordered_map>

#include <win32_helper.hpp>
#include <constexpr_map.hpp>

namespace spirv_parser {

using word = uint32_t;
using id = word;
using operand = word;

template<bool HasResult=false, bool HasInstructionType=false, uint32_t OperandsSize=0>
struct instruction {
    word word_count;
    word opcode;
    [[no_unique_address]]
    cpp_helper::valid_if_t<HasResult, id> result;
    [[no_unique_address]]
    cpp_helper::valid_if_t<HasInstructionType, word> type;
    [[no_unique_address]]
    cpp_helper::valid_if_t<OperandsSize!=0, std::array<operand,OperandsSize>> operands;
};

static_assert(
    std::is_same_v<decltype(instruction<>{}.result), cpp_helper::empty_type> &&
    std::is_same_v<decltype(instruction<true>{}.result), id>
);

struct instruction_parser {
    std::span<word> words;
    auto get_word_count() {
        return words[0];
    }
    auto get_opcode() {
        return words[1];
    }
};

using decoration = std::unordered_map<id, word>;

enum class execution_model {
    opencl_kernel,
    vertex,
    tesselation_control,
    tesselation_evalution,
    geometry,
    fragment,
    compute,
};

constexpr auto vertex_processor_models = std::to_array<execution_model>({
    execution_model::vertex,
    execution_model::tesselation_control,
    execution_model::tesselation_evalution,
    execution_model::geometry,
});
constexpr bool is_vertex_processor(execution_model model) {
    auto map = constexpr_map::construct_const_map < vertex_processor_models, decltype([](auto d) { return d; }) >();
    return map[model] == model;
}

enum class execution_mode {

};

auto open_spirv_file(std::filesystem::path path) {
    auto file_mapping = win32_helper::map_file(path);
    auto spirv_code = std::span{reinterpret_cast<const uint32_t*>(file_mapping.data()), file_mapping.size()/sizeof(uint32_t)};
    return std::pair{std::move(file_mapping), spirv_code};
}

}