#include <algorithm>
#include <charconv>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <string>
#include <utility>
#include <floatconv/floating_to_chars.h>

namespace fmt {
namespace detail {

template<std::size_t N>
struct StringLiteral
{
    constexpr static const size_t Size = N;

    constexpr StringLiteral(const char (&str)[N])
    {
        std::copy_n(str, N, value);
    }

    char value[N];
};

struct FormatArg
{
public:
    enum class Type
    {
        None,
        Binary,
        Decimal,
        Octal,
        Hex,
        Scientific,
        Fixed,
        General
    };

    constexpr FormatArg() = default;

    int begin = -1;
    int end = -1;
    int position = -1;
    int width = -1;
    int precision = -1;
    Type type = Type::None;
    bool alternate = false;
};

template<StringLiteral formatString, size_t NumArgs>
consteval std::array<FormatArg, NumArgs> parseArgs()
{
    if constexpr (NumArgs == 0)
        return {};

    std::array<FormatArg, NumArgs> args;

    enum ArgState {
        Outside,
        Position,
        Width,
        Precision,
        Type
    };
    size_t inarg = 0;
    size_t offset = 0;
    size_t argno = 0;

    auto parseInt = [](size_t begin, size_t end) -> int {
        int v = 0, o = 1;
        while (end > begin) {
            if (formatString.value[end - 1] < '0' || formatString.value[end - 1] > '9')
                return v;
            v += (formatString.value[end - 1] - '0') * o;
            o *= 10;
            --end;
        }
        return v;
    };

    ArgState argState = Outside;

    auto finalize = [&argState, &argno, &args, &parseInt](size_t begin, size_t end) {
        switch (argState) {
        case Position:
            args[argno].position = parseInt(begin, end);
            break;
        case Width:
            args[argno].width = parseInt(begin, end);
            break;
        case Precision:
            args[argno].precision = parseInt(begin, end);
            break;
        default:
            break;
        }
    };

    for (size_t i = 0; i < formatString.Size; ++i)
    {
        switch (formatString.value[i]) {
        case '{':
            if (!inarg++) {
                if (i + 1 < formatString.Size && formatString.value[i + 1] == '{') {
                    --inarg;
                    ++i;
                } else {
                    argState = Position;
                    offset = i + 1;
                    args[argno].begin = i;
                }
            }
            break;
        case ':':
            if (argState != Outside) {
                if (argState >= Width)
                    throw "Only one ':' allowed";
                args[argno].position = parseInt(offset, i);
                argState = Width;
                offset = i + 1;
            }
            break;
        case '.':
            if (argState != Outside) {
                if (argState >= Precision)
                    throw "Only one '.' allowed";
                args[argno].width = parseInt(offset, i);
                argState = Precision;
                offset = i + 1;
            }
            break;
        case 'b':
            if (argState != Outside) {
                finalize(offset, i);
                if (argState >= Type)
                    throw "Only one type allowed";
                args[argno].type = FormatArg::Type::Binary;
                argState = Type;
                offset = i + 1;
            }
            break;
        case 'd':
            if (argState != Outside) {
                finalize(offset, i);
                if (argState >= Type)
                    throw "Only one type allowed";
                args[argno].type = FormatArg::Type::Decimal;
                argState = Type;
                offset = i + 1;
            }
            break;
        case 'o':
            if (argState != Outside) {
                finalize(offset, i);
                if (argState >= Type)
                    throw "Only one type allowed";
                args[argno].type = FormatArg::Type::Octal;
                argState = Type;
                offset = i + 1;
            }
            break;
        case 'h':
            [[fallthrough]];
        case 'a':
            if (argState != Outside) {
                finalize(offset, i);
                if (argState >= Type)
                    throw "Only one type allowed";
                args[argno].type = FormatArg::Type::Hex;
                argState = Type;
                offset = i + 1;
            }
            break;
        case 'e':
            if (argState != Outside) {
                finalize(offset, i);
                if (argState >= Type)
                    throw "Only one type allowed";
                args[argno].type = FormatArg::Type::Scientific;
                argState = Type;
                offset = i + 1;
            }
            break;
        case 'f':
            if (argState != Outside) {
                finalize(offset, i);
                if (argState >= Type)
                    throw "Only one type allowed";
                args[argno].type = FormatArg::Type::Fixed;
                argState = Type;
                offset = i + 1;
            }
            break;
        case 'g':
            if (argState != Outside) {
                finalize(offset, i);
                if (argState >= Type)
                    throw "Only one type allowed";
                args[argno].type = FormatArg::Type::General;
                argState = Type;
                offset = i + 1;
            }
            break;
        case '#':
            if (argState != Outside) {
                args[argno].alternate = true;
                offset = i + 1;
            }
            break;
        case '}':
            if (inarg > 0) {
                if (!--inarg) {
                    finalize(offset, i);
                    argState = Outside;
                    args[argno].end = i + 1;
                    offset = i + 1;
                    ++argno;
                }
            }
        }
    }

    if (args[0].begin == -1 || args[NumArgs - 1].end == -1)
        throw "Args parse error";

    return args;
}

template<StringLiteral formatString>
consteval size_t argumentCount()
{
    size_t cnt = 0;
    size_t inarg = 0;
    for (size_t i = 0; i < formatString.Size; ++i)
    {
        switch (formatString.value[i]) {
        case '{':
            if (!inarg++) {
                if (i + 1 < formatString.Size && formatString.value[i + 1] == '{') {
                    --inarg;
                    ++i;
                } else {
                    ++cnt;
                }
            }
            break;
        case '}':
            if (inarg > 0)
                --inarg;
        }
    }
    return cnt;
}

template<typename OutputIt, size_t N>
constexpr void format_to_type(OutputIt out, const FormatArg& fmt, const char (&argument)[N])
{
    static_assert(N > 0);
    std::copy(argument, argument + N - 1, out);
}

template<typename OutputIt, typename Arg>
constexpr std::enable_if_t<std::is_floating_point_v<std::decay_t<Arg>>> format_to_type(OutputIt out, const FormatArg& fmt, Arg argument)
{
    char buffer[64];

    std::chars_format cfmt;
    switch (fmt.type) {
    case FormatArg::Type::Hex:
        cfmt = std::chars_format::hex;
        break;
    case FormatArg::Type::Scientific:
        cfmt = std::chars_format::scientific;
        break;
    case FormatArg::Type::General:
        cfmt = std::chars_format::general;
        break;
    default:
        cfmt = std::chars_format::fixed;
        break;
    }

    auto ret = floatconv::to_chars(buffer, buffer + sizeof(buffer), static_cast<double>(argument), cfmt, fmt.precision);
    if (ret.ec == std::errc {}) {
        std::copy(buffer, ret.ptr, out);
    }
}

template<typename OutputIt, typename Arg>
constexpr std::enable_if_t<std::is_integral_v<std::decay_t<Arg>>> format_to_type(OutputIt out, const FormatArg& fmt, Arg argument)
{
    if constexpr (std::is_same_v<std::decay_t<Arg>, bool>) {
        if (argument) {
            std::copy_n("true", 4, out);
        } else {
            std::copy_n("false", 5, out);
        }
    } else {
        char buffer[64];
        const char* alt = nullptr;
        int altsz = 0;

        int base;
        switch (fmt.type) {
        case FormatArg::Type::Binary:
            alt = "0b";
            altsz = 2;
            base = 2;
            break;
        case FormatArg::Type::Octal:
            alt = "0";
            altsz = 1;
            base = 10;
            break;
        case FormatArg::Type::Hex:
            alt = "0x";
            altsz = 2;
            base = 16;
            break;
        default:
            base = 10;
            break;
        }

        auto ret = std::to_chars(buffer, buffer + sizeof(buffer), argument, base);
        if (ret.ec == std::errc {}) {
            if (fmt.alternate && alt) {
                std::copy_n(alt, altsz, out);
            }
            std::copy(buffer, ret.ptr, out);
        }
    }
}

template<StringLiteral formatStr, size_t ArgIdx, size_t ArgCount, typename OutputIt, typename Formats>
constexpr void copy_next(OutputIt out, const Formats& formats)
{
    if constexpr (ArgIdx + 1 >= ArgCount)
        return;
    std::copy(formatStr.value + formats[ArgIdx].end, formatStr.value + formats[ArgIdx + 1].begin, out);
}

template<StringLiteral formatStr, typename OutputIt, typename Formats, uint32_t... Is, typename... Args>
constexpr void format_to_internal(OutputIt out, const Formats& formats, std::integer_sequence<uint32_t, Is...>, Args&&... args)
{
    ( (format_to_type(out, std::get<Is>(formats), args), copy_next<formatStr, Is, std::tuple_size_v<Formats>>(out, formats)), ...);
}

} // namespace detail

template<detail::StringLiteral formatStr, typename OutputIt, typename... Args>
constexpr void format_to(OutputIt out, Args&&... args)
{
    static_assert(detail::argumentCount<formatStr>() == sizeof...(Args));
    if constexpr (sizeof...(Args) == 0) {
        return;
    } else {
        auto argfmts = detail::parseArgs<formatStr, sizeof...(Args)>();
        std::copy(formatStr.value, formatStr.value + argfmts[0].begin, out);
        detail::format_to_internal<formatStr>(out, argfmts, std::make_integer_sequence<uint32_t, sizeof...(Args)>{}, std::forward<Args>(args)...);
        std::copy(formatStr.value + argfmts[sizeof...(Args) - 1].end, formatStr.value + (formatStr.Size + 1), out);
    }
}

template<detail::StringLiteral formatStr, typename... Args>
constexpr std::string format(Args&&... args)
{
    std::string str;
    // guesstimate, will need to collect some data to see if this makes sense
    str.reserve(formatStr.Size + (sizeof...(Args) * 5));
    format_to<formatStr>(std::back_inserter(str), std::forward<Args>(args)...);
    return str;
}

} // namespace fmt
