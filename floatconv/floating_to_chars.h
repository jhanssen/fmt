#include <charconv>

namespace floatconv {
// Overloads for float.
std::to_chars_result to_chars(char* __first, char* __last, float __value) noexcept;
std::to_chars_result to_chars(char* __first, char* __last, float __value,
                              std::chars_format __fmt) noexcept;
std::to_chars_result to_chars(char* __first, char* __last, float __value,
                              std::chars_format __fmt, int __precision) noexcept;

// Overloads for double.
std::to_chars_result to_chars(char* __first, char* __last, double __value) noexcept;
std::to_chars_result to_chars(char* __first, char* __last, double __value,
                              std::chars_format __fmt) noexcept;
std::to_chars_result to_chars(char* __first, char* __last, double __value,
                              std::chars_format __fmt, int __precision) noexcept;

// Overloads for long double.
std::to_chars_result to_chars(char* __first, char* __last, long double __value) noexcept;
std::to_chars_result to_chars(char* __first, char* __last, long double __value,
                              std::chars_format __fmt) noexcept;
std::to_chars_result to_chars(char* __first, char* __last, long double __value,
                              std::chars_format __fmt, int __precision) noexcept;
}
