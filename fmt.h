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

    constexpr FormatArg()
        : begin(-1), end(-1), position(-1), width(-1), precision(-1), type(Type::None)
    {
    }

    constexpr FormatArg(int b, int e, int p, int w = 0, int pr = -1, Type t = Type::None)
        : begin(b), end(e), position(p), precision(pr), type(t)
    {
    }

    int begin;
    int end;
    int position;
    int width;
    int precision;
    Type type;
};

struct Piece
{
    Piece();
    template<size_t N>
    constexpr Piece(const char (&str)[N]);
    Piece(const char* str, size_t sz);
    Piece(Piece&& other);
    Piece(const Piece&) = delete;
    ~Piece();

    Piece& operator=(Piece&& other);
    Piece& operator=(const Piece&) = delete;

    void updateSize(size_t sz);
    char* ensure(size_t size);
    std::string_view view() const;

    enum { PieceLength = 32 };

    char stackBuffer[PieceLength];
    char* buffer = nullptr;
    size_t size = 0;
};

inline Piece::Piece()
    : buffer(stackBuffer)
{
}

template<size_t N>
constexpr Piece::Piece(const char (&str)[N])
    : buffer(stackBuffer), size(N - 1)
{
    static_assert(N > 0 && N <= PieceLength);
    std::copy_n(str, N, buffer);
}

inline Piece::Piece(const char* str, size_t sz)
    : size(sz)
{
    if (sz <= PieceLength) {
        buffer = stackBuffer;
    } else {
        buffer = new char[sz];
    }
    memcpy(buffer, str, sz);
}

inline Piece::Piece(Piece&& other)
    : buffer(stackBuffer)
{
    if (other.buffer == other.stackBuffer) {
        if (other.size > 0)
            memcpy(buffer, other.buffer, other.size);
        size = other.size;
    } else {
        size = other.size;
        buffer = other.buffer;
        other.buffer = other.stackBuffer;
    }
}

inline Piece::~Piece()
{
    if (buffer != stackBuffer)
        delete[] buffer;
}

inline Piece& Piece::operator=(Piece&& other)
{
    if (buffer != stackBuffer)
        delete[] buffer;
    if (other.buffer == other.stackBuffer) {
        buffer = stackBuffer;
        if (other.size > 0)
            memcpy(buffer, other.buffer, other.size);
        size = other.size;
    } else {
        size = other.size;
        buffer = other.buffer;
        other.buffer = other.stackBuffer;
    }
    return *this;
}

inline void Piece::updateSize(size_t sz)
{
    assert(sz <= size || (buffer == stackBuffer && sz <= PieceLength));
    if (sz > size)
        size = sz;
}

inline char* Piece::ensure(size_t sz)
{
    if (buffer == stackBuffer && sz <= PieceLength) {
        size = sz;
        return buffer;
    }
    if (sz <= size)
        return buffer;
    if (buffer != stackBuffer)
        delete[] buffer;
    buffer = new char[sz];
    size = sz;
    return buffer;
}

inline std::string_view Piece::view() const
{
    return std::string_view(buffer, size);
}

template<typename Arg>
inline std::enable_if_t<std::is_floating_point_v<std::decay_t<Arg>>, Piece> makePiece(const FormatArg& fmt, Arg argument)
{
    Piece piece;
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
    auto ret = floatconv::to_chars(piece.buffer, piece.buffer + Piece::PieceLength, static_cast<double>(argument), cfmt, fmt.precision);
    if (ret.ec == std::errc {}) {
        piece.updateSize(ret.ptr - piece.buffer);
    }
    return piece;
}

template<typename Arg>
inline std::enable_if_t<std::is_integral_v<std::decay_t<Arg>>, Piece> makePiece(const FormatArg& fmt, Arg argument)
{
    using DecayedArg = std::decay_t<Arg>;
    if constexpr (std::is_same_v<DecayedArg, bool>) {
        return argument ? Piece("true") : Piece("false");
    } else if constexpr (std::is_same_v<DecayedArg, int64_t> || std::is_same_v<DecayedArg, uint64_t>) {
        Piece piece;
        const auto end = fastIntToBuffer(argument, piece.buffer);
        piece.updateSize(end - piece.buffer);
        return piece;
    } else if constexpr (std::is_unsigned_v<DecayedArg>) {
        Piece piece;
        auto ret = std::to_chars(piece.buffer, piece.buffer + Piece::PieceLength, static_cast<uint32_t>(argument), 10);
        if (ret.ec == std::errc {}) {
            piece.updateSize(ret.ptr - piece.buffer);
        }
        return piece;
    } else {
        Piece piece;
        auto ret = std::to_chars(piece.buffer, piece.buffer + Piece::PieceLength, static_cast<int32_t>(argument), 10);
        if (ret.ec == std::errc {}) {
            piece.updateSize(ret.ptr - piece.buffer);
        }
        return piece;
    }
}

inline Piece makePiece(const FormatArg& fmt, std::string_view argument)
{
    return Piece(argument.data(), argument.size());
}

template<size_t N>
inline Piece makePiece(const FormatArg& fmt, const char (&argument)[N])
{
    return Piece(argument);
}

template<typename Formats, typename Tuple, uint32_t... Is>
inline std::array<Piece, sizeof...(Is)> makePieces_internal(Formats& formats, std::integer_sequence<uint32_t, Is...>, Tuple& t)
{
    return {{ { makePiece(formats[Is], std::get<Is>(t)) }... }};
}

template<typename Formats, typename... Args>
inline std::array<Piece, sizeof...(Args)> makePieces(Formats& formats, Args&&... args)
{
    auto data = std::make_tuple(std::forward<Args>(args)...);
    return makePieces_internal(formats, std::make_integer_sequence<uint32_t, sizeof...(Args)>{}, data);
}

template<StringLiteral formatString, size_t NumArgs>
consteval std::array<FormatArg, NumArgs> parseArgs()
{
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

template<StringLiteral lit, size_t ArgIdx, size_t ArgCount, typename OutputIt, typename Formats>
constexpr void copy_next(OutputIt out, const Formats& formats)
{
    if constexpr (ArgIdx + 1 >= ArgCount)
        return;
    std::copy(lit.value + formats[ArgIdx].end, lit.value + formats[ArgIdx + 1].begin, out);
}

template<StringLiteral lit, typename OutputIt, typename Formats, uint32_t... Is, typename... Args>
constexpr void format_to_internal(OutputIt out, const Formats& formats, std::integer_sequence<uint32_t, Is...>, Args&&... args)
{
    ( (format_to_type(out, std::get<Is>(formats), args), copy_next<lit, Is, std::tuple_size_v<Formats>>(out, formats)), ...);
}

} // namespace detail

template<detail::StringLiteral lit, typename OutputIt, typename... Args>
constexpr void format_to(OutputIt out, Args&&... args)
{
    static_assert(detail::argumentCount<lit>() == sizeof...(Args));
    if constexpr (sizeof...(Args) == 0) {
        return;
    } else {
        auto argfmts = detail::parseArgs<lit, sizeof...(Args)>();
        // auto argdatas = std::make_tuple(std::forward<Args>(args)...);
        std::copy(lit.value, lit.value + argfmts[0].begin, out);
        detail::format_to_internal<lit>(out, argfmts, std::make_integer_sequence<uint32_t, sizeof...(Args)>{}, std::forward<Args>(args)...);
        std::copy(lit.value + argfmts[sizeof...(Args) - 1].end, lit.value + (lit.Size + 1), out);
    }
}

template<detail::StringLiteral lit, typename... Args>
constexpr std::string format(Args&&... args)
{
    std::string str;
    format_to<lit>(std::back_inserter(str), std::forward<Args>(args)...);
    return str;
}

} // namespace fmt
