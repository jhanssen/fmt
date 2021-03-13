#include <algorithm>
#include <charconv>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <string>
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
inline std::enable_if_t<std::is_floating_point_v<std::decay_t<Arg>>, Piece> makePiece(Arg argument)
{
    Piece piece;
    auto ret = floatconv::to_chars(piece.buffer, piece.buffer + Piece::PieceLength, static_cast<double>(argument), std::chars_format::fixed, 6);
    if (ret.ec == std::errc {}) {
        piece.updateSize(ret.ptr - piece.buffer);
    }
    return piece;
}

template<typename Arg>
inline std::enable_if_t<std::is_integral_v<std::decay_t<Arg>>, Piece> makePiece(Arg argument)
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

inline Piece makePiece(std::string_view argument)
{
    return Piece(argument.data(), argument.size());
}

template<size_t N>
inline Piece makePiece(const char (&argument)[N])
{
    return Piece(argument);
}

} // namespace detail

template<detail::StringLiteral lit, typename OutputIt, typename... Args>
void format_to(OutputIt out, Args&&... args)
{
}

template<detail::StringLiteral lit, typename... Args>
std::string format(Args&&... args)
{
    std::string str;
    format_to<lit>(std::back_inserter(str), std::forward<Args>(args)...);
    return str;
}

} // namespace fmt
