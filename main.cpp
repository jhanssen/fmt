#include <fmt.h>

int main(int, char**)
{
    auto str = fmt::format<"foo{:.3a}b {}oho{h}">(123.4, "abc", 15);
    printf("yay '%s'\n", str.c_str());
    return 0;
}
