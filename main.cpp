#include <fmt.h>

int main(int, char**)
{
    auto str = fmt::format<"foo {}">(123);
    printf("yay '%s'\n", str.c_str());
    return 0;
}
