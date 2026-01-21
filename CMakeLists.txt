cmake_minimum_required(VERSION 3.20)

project(SPIRV-Parser)

add_executable(SPIRV-Parser
    spirv_parser.cpp
)

target_link_libraries(SPIRV-Parser PUBLIC SPIRV-Headers)