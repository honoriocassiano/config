#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#define internal static
#define static_global static
#define static_local static

using byte  = std::uint8_t;
using i16   = std::int16_t;
using u16   = std::uint16_t;
using i32   = std::int32_t;
using u32   = std::uint32_t;
using i64   = std::int64_t;
using u64   = std::uint64_t;
using isize = std::ptrdiff_t;
using usize = std::size_t;

using f32 = float;
using f64 = double;

#define STR_(str) #str
#define STR(str) STR_(str)
#define COUNT_X_MACRO(...)  +1

#define LOG(fmt, ...) std::printf("%s:%d: (%s) " fmt "\n", __FILE__, __LINE__, __PRETTY_FUNCTION__, ##__VA_ARGS__)
#define DEBUG(fmt, cond) LOG(STR(cond) ": " fmt, cond)

#define ASSERT(...) do { if(!(__VA_ARGS__)) { LOG("A asserção \'" STR(__VA_ARGS__) "\' falhou"); std::exit(-1); } } while(0)


#define EMBED_SOURCE_FILE(c_name, f_name)          \
asm(".section .rodata;"                            \
    ".global " #c_name "_file_start;"              \
    ".global " #c_name "_file_end;"                \
    ".type " #c_name "_file_start, @object;"       \
    ".type " #c_name "_file_end, @object;"         \
    ".align 1;"                                    \
    #c_name "_file_start: .incbin \"" f_name "\";" \
    #c_name "_file_end: .byte 0;"                  \
    ".previous;");                                 \
extern const byte c_name ## _file_start;           \
extern const byte c_name ## _file_end;             \
namespace source {                                 \
    static_global const byte * const c_name ## _file_content = &c_name ## _file_start;                        \
    static_global const usize        c_name ## _file_size    = &c_name ## _file_end - &c_name ## _file_start; \
}

#define EMBED_SOURCE_FILE_LIST(c_name, f_name)  EMBED_SOURCE_FILE(c_name, f_name)
#define SOURCE_FILE_C_NAME_LIST(c_name, f_name) source::c_name ## _file_content,
#define SOURCE_FILE_F_NAME_LIST(c_name, f_name) f_name,
#define SOURCE_FILE_CONTENT_SIZE_LIST(c_name, f_name)  source:: c_name ## _file_size,


#error "Especificar o restante dos arquivos de código fonte"

#define SOURCE_FILES(X) X(main, __FILE__)

SOURCE_FILES(EMBED_SOURCE_FILE_LIST)


internal auto unpack_source_code() -> void
{
    const byte * const Source_File_Content_List[]      { SOURCE_FILES(SOURCE_FILE_C_NAME_LIST) };
    const char * const Source_Filename_List[]          { SOURCE_FILES(SOURCE_FILE_F_NAME_LIST) };
    const usize        Source_File_Content_Size_List[] { SOURCE_FILES(SOURCE_FILE_CONTENT_SIZE_LIST) };

    for (usize i=0; i<(SOURCE_FILES(COUNT_X_MACRO)); i++)
    {
        char string_buffer[4096];
        {
            i32 folder_size = 0;
            for (auto current_char=Source_Filename_List[i]; *current_char != '\0'; current_char++)
            {
                if (*current_char == '/') { folder_size = current_char-Source_Filename_List[i]; }
            }

            std::snprintf(string_buffer, sizeof(string_buffer), "mkdir -p source_code_/%.*s", folder_size, Source_Filename_List[i]);
            std::system(string_buffer);
        }

        std::snprintf(string_buffer, sizeof(string_buffer), "source_code_/%s", Source_Filename_List[i]);

        FILE * file = std::fopen(string_buffer, "wb");
        if (file == nullptr)
        {
            std::printf("não foi possível criar o arquivo '%s'\n", string_buffer);
            continue;
        }
        std::fwrite(Source_File_Content_List[i], 1, Source_File_Content_Size_List[i], file);
        std::fclose(file);
        std::puts(string_buffer);
    }
}

auto main(i32 argc, const char ** argv) -> i32
{
    if (argc == 2 && std::strcmp(argv[1], "--unpack-source-code") == 0)
    {
        unpack_source_code();
        return 0;
    }
}
// g++ -std=c++26 -nostdlib++ -fno-exceptions -fno-rtti -Wall -Wextra -Wpedantic -Wno-unused-function -o main main.cpp

