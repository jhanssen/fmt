#include <fmt.h>

int main(int, char**)
{
    auto str = fmt::format<"foo {:.3a}">(123.4);
    printf("yay '%s'\n", str.c_str());
    return 0;
}
