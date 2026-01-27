#include <spirv/unified1/spirv.hpp>

#include <unordered_map>
#include <tuple>

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

struct instruction_binary {
    std::vector<word> words;
    auto get_word_count() const {
        return (words[0] >> 16) & 0xffff;
    }
    auto get_opcode() const {
        return static_cast<spv::Op>(words[0] & 0xffff);
    }
};

struct instruction_binary_ref {
    word* words;
    auto get_word_count() const {
        return (words[0] >> 16) & 0xffff;
    }
    auto get_opcode() const {
        return static_cast<spv::Op>(words[0] & 0xffff);
    }
};

enum class instruction_argument {
    none,
    capability,
    id,
    execution_mode,
    literal_string,
    memory_model,
    addressing_model,
};

struct instruction_encode {
    constexpr instruction_encode() = default;
    constexpr instruction_encode(spv::Op op, instruction_argument arg)
    : op{op},
      args{arg}
    {}
    constexpr instruction_encode(spv::Op op, instruction_argument arg0, instruction_argument arg1)
    : op{op},
      args{arg0, arg1}
    {}
    spv::Op op;
    instruction_argument args[4];
};

constexpr auto instruction_encodes = std::to_array<instruction_encode>({
    {spv::OpCapability, instruction_argument::capability},
    {spv::OpExtInstImport, instruction_argument::id, instruction_argument::literal_string},
    {spv::OpMemoryModel, instruction_argument::addressing_model, instruction_argument::memory_model},
    //{spv::OpEntryPoint, instruction_argument::execution_model, instruction_argument::id, instruction_argument::literal_string, instruction_argument::ids},
});
constexpr auto get_instruction_encode(spv::Op op) {
    constexpr auto map = constexpr_map::construct_const_map<instruction_encodes, decltype([](auto i) {return i.op; })>();
    return map[op];
}

std::ostream& operator<<(std::ostream& out, const spv::Capability cap) {
    return out << spv::CapabilityToString(cap);
}

std::ostream& operator<<(std::ostream& out, const spv::MemoryModel memory_model) {
    return out << spv::MemoryModelToString(memory_model);
}

std::ostream& operator<<(std::ostream& out, const spv::AddressingModel addressing_model) {
    return out << spv::AddressingModelToString(addressing_model);
}

std::ostream& operator<<(std::ostream& out, const instruction_binary_ref& inst) {
    out << spv::OpToString(inst.get_opcode());
    const auto encode = get_instruction_encode(inst.get_opcode());
    if (encode.op != inst.get_opcode()) {
        throw std::runtime_error{"unknow opcode"};
    }
    auto word = inst.words+1;
    for (const auto& arg : encode.args) {
        if (arg == instruction_argument::capability) {
            out << " " << static_cast<spv::Capability>(*word);
            ++word;
        }
        else if (arg == instruction_argument::literal_string) {
            out << " " << reinterpret_cast<char*>(word);
        }
        else if (arg == instruction_argument::id) {
            out << " %" << *word;
            ++word;
        }
        else if (arg == instruction_argument::none) {
            break;
        }
        else if (arg == instruction_argument::memory_model) {
            out << " " << static_cast<spv::MemoryModel>(*word);
            ++word;
        }
        else if (arg == instruction_argument::addressing_model) {
            out << " " << static_cast<spv::AddressingModel>(*word);
            ++word;
        }
        else {
            out << " " << "unknown argument";
        }
    }
    return out;
}

struct instruction_binary_iterator {
    word* words;

    auto operator*() {
        return instruction_binary_ref{{words}};
    }
    auto operator++() {
        words = words + (**this).get_word_count();
    }
};

auto operator==(const instruction_binary_iterator& lhs, const instruction_binary_iterator& rhs) {
    return lhs.words == rhs.words;
}

auto operator<=>(const instruction_binary_iterator& lhs, const instruction_binary_iterator& rhs) {
    return lhs.words <=> rhs.words;
}

struct block {
    std::span<word> words;
    auto begin() {
        return instruction_binary_iterator{words.data()};
    }
    auto end() {
        return instruction_binary_iterator{words.data() + words.size()};
    }
};

constexpr auto function_termination_instructions = std::to_array<spv::Op>({
    spv::OpReturn,
    spv::OpReturnValue,
    spv::OpKill,
    spv::OpUnreachable,
    spv::OpTerminateInvocation,
});
constexpr bool is_function_termination_instruction(spv::Op op) {
    auto set = constexpr_map::construct_const_set<function_termination_instructions>();
    return set[op] == op;
}

constexpr auto conditional_branch_instructions = std::to_array<spv::Op>({
    spv::OpBranchConditional,
    spv::OpSwitch
});
constexpr bool is_conditional_branch_instruction(spv::Op op) {
    auto set = constexpr_map::construct_const_set<conditional_branch_instructions>();
    return set[op] == op;
}
constexpr auto branch_instructions = cpp_helper::merge(
    std::to_array<spv::Op>({
        spv::OpBranch,
    }),
    conditional_branch_instructions
);
constexpr bool is_branch_instruction(spv::Op op) {
    auto set = constexpr_map::construct_const_set<branch_instructions>();
    return set[op] == op;
}

constexpr auto block_termination_instructions = cpp_helper::merge(
    branch_instructions,
    function_termination_instructions
);
constexpr bool is_block_termination_instruction(spv::Op op) {
    auto set = constexpr_map::construct_const_set<block_termination_instructions>();
    return set[op] == op;
}

constexpr auto merge_instructions = std::to_array<spv::Op>({
    spv::OpSelectionMerge,
    spv::OpLoopMerge
});
constexpr bool is_merge_instruction(spv::Op op) {
    auto set = constexpr_map::construct_const_set<merge_instructions>();
    return set[op] == op;
}

bool is_header_block(block b) {
    return std::any_of(b.begin(), b.end(), [](auto i){ return is_merge_instruction(i.get_opcode()); });
}
bool is_loop_header(block b) {
    return std::any_of(b.begin(), b.end(), [](auto i){ return i.get_opcode() == spv::OpLoopMerge; });
}
bool is_selection_header(block b) {
    return std::any_of(b.begin(), b.end(), [](auto i){ return i.get_opcode() == spv::OpSelectionMerge; }) &&
            std::any_of(b.begin(), b.end(), [](auto i){ return i.get_opcode() == spv::OpBranchConditional; });
}
bool is_switch_header(block b) {
    return std::any_of(b.begin(), b.end(), [](auto i){ return i.get_opcode() == spv::OpSelectionMerge; }) &&
            std::any_of(b.begin(), b.end(), [](auto i){ return i.get_opcode() == spv::OpSwitch; });
}

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
    auto set = constexpr_map::construct_const_set<vertex_processor_models>();
    return set[model] == model;
}

constexpr auto execution_model_capabilities = std::to_array<std::pair<spv::ExecutionModel,spv::Capability>>({
    {spv::ExecutionModelVertex, spv::CapabilityShader},
    {spv::ExecutionModelTessellationControl, spv::CapabilityTessellation},
    {spv::ExecutionModelTessellationEvaluation, spv::CapabilityTessellation},
    {spv::ExecutionModelGeometry, spv::CapabilityGeometry},
    {spv::ExecutionModelFragment, spv::CapabilityShader},
    {spv::ExecutionModelGLCompute, spv::CapabilityShader},
    {spv::ExecutionModelKernel, spv::CapabilityKernel},
    {spv::ExecutionModelTaskEXT, spv::CapabilityMeshShadingEXT},
    {spv::ExecutionModelMeshEXT, spv::CapabilityMeshShadingEXT},
});

constexpr auto addressing_model_capabilities = std::to_array<std::pair<spv::AddressingModel, spv::Capability>>({
    {spv::AddressingModelPhysical32, spv::CapabilityAddresses},
    {spv::AddressingModelPhysical64, spv::CapabilityAddresses},
    {spv::AddressingModelPhysicalStorageBuffer64, spv::CapabilityPhysicalStorageBufferAddresses},
});

constexpr auto memory_model_capabilities = std::to_array<std::pair<spv::MemoryModel, spv::Capability>>({
    {spv::MemoryModelSimple, spv::CapabilityShader},
    {spv::MemoryModelGLSL450, spv::CapabilityShader},
    {spv::MemoryModelOpenCL, spv::CapabilityKernel},
    {spv::MemoryModelVulkan, spv::CapabilityVulkanMemoryModel},
});

enum class execution_mode {

};

class control_flow_graph {
    class node{};
    std::vector<node> nodes;
};

struct module_binary {
    std::span<word> words;
    auto get_magic_number() {
        return words[0];
    }
    auto get_version_number() {
        return words[1];
    }
    auto get_generator_magic_number() {
        return words[2];
    }
    auto get_id_bound() {
        return words[3];
    }
    auto begin() {
        return instruction_binary_iterator(words.data() + 5);
    }
    auto end() {
        return instruction_binary_iterator(words.data() + words.size());
    }
};

struct module_logic {
    std::vector<word> capabilities;
    std::vector<word> extensions;
    std::vector<word> extension_instruction_imports;
    word memory_model;
    std::vector<word> entry_points;
    std::vector<word> execution_modes;
    std::vector<word> strings;
    std::vector<word> names;
    std::vector<word> module_porcesseds;
    std::vector<word> annotations;
    std::vector<word> declarations;
    std::vector<word> function_declarations;
    std::vector<word> function_definitions;
};

auto open_spirv_file(std::filesystem::path path) {
    auto file_mapping = win32_helper::map_file(path);
    auto spirv_code = std::span{reinterpret_cast<word*>(file_mapping.data()), file_mapping.size()/sizeof(word)};

    auto m = spirv_parser::module_binary{ spirv_code };
    if (m.get_magic_number() != spv::MagicNumber) {
        throw std::runtime_error{ std::format("file magic number is {} != {}", m.get_magic_number(), spv::MagicNumber) };
    }

    return std::pair{std::move(file_mapping), m};
}

}