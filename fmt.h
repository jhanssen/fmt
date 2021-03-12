#include <algorithm>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <string>

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

} // namespace detail

template<detail::StringLiteral lit, typename OutputIt, typename... Args>
void format_to(OutputIt out, const Args&... args)
{
}

template<detail::StringLiteral lit, typename... Args>
std::string format(const Args&... args)
{
    std::string str;
    format_to(std::back_inserter(str), std::forward<Args>(args)...);
    return str;
}

} // namespace fmt
