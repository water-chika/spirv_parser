#include "spirv_parser.hpp"
#include <iostream>
int main() {
    try {
        auto [file_mapping, spirv_code] = spirv_parser::open_spirv_file("test.spv");
        auto module_binary = spirv_parser::module_binary{spirv_code};

        spirv_parser::word count = 0;
        std::for_each(module_binary.begin(), module_binary.end(),
            [&count](auto i){
                ++count;
                std::cout << spv::OpToString(i.get_opcode()) << std::endl;
            });
        std::cout << "spirv file size(bytes):" << spirv_code.size() << std::endl;
        std::cout << "instruction count: " << count << std::endl;
    }
    catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
    return 0;
}