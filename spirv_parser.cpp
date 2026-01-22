#include "spirv_parser.hpp"
#include <iostream>
int main() {
    try {
        auto [file_mapping, spirv_code] = spirv_parser::open_spirv_file("test.spv");
        std::cout << spirv_code.size() << std::endl;
    }
    catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
    return 0;
}