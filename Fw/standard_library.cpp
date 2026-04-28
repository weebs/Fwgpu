#include <iostream>
#include <string>
namespace System {
namespace Console {
void WriteLine(std::string s) { std::cout << s << std::endl; }
void WriteLine(int n) { std::cout << n << std::endl; }
} // namespace Console
} // namespace System
namespace Microsoft {
namespace FSharp {
namespace Core {
namespace Operators {
int op_Addition(int x, int y) { return x + y; }
// std::string ToString(int x) { return std::string(""); }
template <typename T> std::string ToString(T x) { return std::to_string(x); }
} // namespace Operators
} // namespace Core
} // namespace FSharp
} // namespace Microsoft