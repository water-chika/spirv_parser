cmake_minimum_required(VERSION 3.20)

project(spirv_parser)

add_library(spirv_parser INTERFACE
    spirv_parser.hpp
)

target_include_directories(spirv_parser INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(spirv_parser INTERFACE
    SPIRV-Headers
    cpp_helper
    constexpr_map
    map_file
)

target_compile_features(spirv_parser INTERFACE cxx_std_20)

target_compile_definitions(spirv_parser INTERFACE SPV_ENABLE_UTILITY_CODE)

add_executable(spirv_parser_test
    spirv_parser.cpp
)

target_link_libraries(spirv_parser_test PUBLIC
    spirv_parser
)

