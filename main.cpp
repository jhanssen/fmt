#include <fmt.h>

int main(int, char**)
{
    auto str = fmt::format<"foo{:.3a}b {}oho{}">(123.4, "abc", 9.9);
    printf("yay '%s'\n", str.c_str());
    return 0;
}
