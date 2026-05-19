#include "spirv_parser.hpp"
#include <iostream>
#include <map>
#include <filesystem>
#include <fstream>
#include <cstring>

using namespace spirv_parser;
int main(int argc, const char** argv) {
    try {
        if (argc < 1) {
            throw std::runtime_error{std::format("usage: {} <spirv_file_path>", argv[0])};
        }
        std::filesystem::path path = argv[1];
        if (!exists(path)) {
            throw std::runtime_error{"spirv file not exitst"};
        }
        auto [file_mapping, module_binary] = spirv_parser::open_spirv_file(argv[1]);

        bool dump_shader_sources = argc > 2;
        std::unordered_map<spirv_parser::id, std::string> strings{};
        std::unordered_map<spirv_parser::id, std::string> sources{};

        spirv_parser::word count = 0;
        std::for_each(module_binary.begin(), module_binary.end(),
            [&count, &strings, &sources](auto inst){
                ++count;
                std::cout << inst << std::endl;
                if (inst.get_opcode() == spv::OpString) {
                    spirv_parser::id id{};
                    std::string string{};
                    const auto encode = get_instruction_encode(inst.get_opcode());
                    auto word = inst.words+1;
                    for (const auto& arg : encode.args) {
                        if (arg == instruction_argument::none || word >= inst.words + inst.get_word_count()) {
                            break;
                        }
                        auto arg_binary_ref = instruction_argument_binary_ref{arg, word, inst.words + inst.get_word_count()};
                        if (arg == instruction_argument::id) {
                            id = {*arg_binary_ref.argument_word};
                        }
                        else if (arg == instruction_argument::literal_string) {
                            string = reinterpret_cast<char*>(arg_binary_ref.argument_word);
                        }
                        word += get_word_count(arg_binary_ref);
                    }
                    strings.emplace(id, string);
                }
                else if (inst.get_opcode() == spv::OpSource) {
                    spirv_parser::id id{};
                    std::string string{};
                    const auto encode = get_instruction_encode(inst.get_opcode());
                    auto word = inst.words+1;
                    for (const auto& arg : encode.args) {
                        if (arg == instruction_argument::none || word >= inst.words + inst.get_word_count()) {
                            break;
                        }
                        auto arg_binary_ref = instruction_argument_binary_ref{arg, word, inst.words + inst.get_word_count()};
                        if (arg == instruction_argument::optional_id) {
                            id = {*arg_binary_ref.argument_word};
                        }
                        else if (arg == instruction_argument::optional_literal_string) {
                            string = reinterpret_cast<char*>(arg_binary_ref.argument_word);
                        }
                        word += get_word_count(arg_binary_ref);
                    }
                    sources.emplace(id, string);
                }
            });
        std::cout << "spirv file size(bytes):" << file_mapping.size() << std::endl;
        std::cout << "instruction count: " << count << std::endl;

        if (dump_shader_sources) {
            for (auto& [k,v] : sources) {
                if (strings[k].size() == 0 || v.size() == 0) continue;
                auto s = std::vector<char>(strings[k].size()+3);
                s[0] = '.';s[1] = '/';
                strcpy(&s[2], strings[k].data());
                std::replace(s.begin(), s.end(), '\\', '/');
                auto path = std::filesystem::path{s.data()};
                auto parent_paths = std::vector{path.parent_path()};
                while (!exists(parent_paths.back())) {
                    parent_paths.emplace_back(parent_paths.back().parent_path());
                    std::cout << parent_paths.back() << std::endl;
                }
                while (!parent_paths.empty()) {
                    create_directory(parent_paths.back());
                    parent_paths.pop_back();
                }
                auto fout = std::ofstream{path};
                fout << v;
                std::cout << path << std::endl;
            }
        }
    }
    catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
    return 0;
}
