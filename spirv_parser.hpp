#include <spirv/unified1/spirv.h>

#include <win32_helper.hpp>

void open_spirv_file(std::filesystem::path path) {
    auto file_mapping = win32_helper::map_file(path);
    auto spirv_code = std::span{reinterpret_cast<const uint32_t*>(file_mapping.data()), file_mapping.size()/sizeof(uint32_t)};
}