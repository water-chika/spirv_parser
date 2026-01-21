cmake_minimum_required(VERSION 3.20)

project(SPIRV-Parser)

add_executable(SPIRV-Parser
    spirv_parser.cpp
)

target_link_libraries(SPIRV-Parser PUBLIC
    SPIRV-Headers
    win32_helper
)

target_compile_features(SPIRV-Parser PUBLIC cxx_std_20)