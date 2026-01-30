#include "spirv_parser.hpp"
#include <iostream>
int main(int argc, const char** argv) {
    try {
        if (argc < 1) {
            throw std::runtime_error{std::format("usage: {} <srirv_file_path>", argv[0])};
        }
        std::filesystem::path path = argv[1];
        auto [file_mapping, module_binary] = spirv_parser::open_spirv_file(argv[1]);

        spirv_parser::word count = 0;
        std::for_each(module_binary.begin(), module_binary.end(),
            [&count](auto i){
                ++count;
                std::cout << i << std::endl;
            });
        std::cout << "spirv file size(bytes):" << file_mapping.size() << std::endl;
        std::cout << "instruction count: " << count << std::endl;
    }
    catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
    return 0;
}