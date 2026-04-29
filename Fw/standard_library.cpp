#include <iostream>
#include <memory>
#include <string>
static std::ios_base::Init stream_initializer;
template <typename T> using Gc = std::shared_ptr<T>;
namespace System {
class IComparable_1 {};
class IComparable {};
class ValueType {};
class IEquatable_1 {};
class Object {
  //   std::string type;

public:
  virtual ~Object() = default;
  //   bool IsType(std::string typeName) { return type == typeName; }
  std::string ToString() { return "System.Object"; }

public:
  void *__data; // TODO : deleting in the destructor
};
template <typename T> bool IsType(Gc<System::Object> obj) {
  return std::dynamic_pointer_cast<T>(obj) != nullptr;
}
class IDisposable {
public:
  virtual ~IDisposable() = default;
  void Dispose() { this->System_IDisposable_Dispose(); }
  virtual void System_IDisposable_Dispose() {};
};
namespace Collections {
class IStructuralEquatable {};
class IStructuralComparable {};
class IComparer {};
class IEqualityComparer {};
} // namespace Collections
namespace Console {
void WriteLine(std::string s) { std::cout << s << std::endl; }
void WriteLine(int n) { std::cout << n << std::endl; }
} // namespace Console
} // namespace System
namespace Microsoft {
namespace FSharp {
namespace Core {
namespace LanguagePrimitives {
Gc<System::Collections::IComparer> GenericComparer;
Gc<System::Collections::IEqualityComparer> GenericEqualityComparer;
template <typename T> bool GenericEqualityER(T a, T b) { return a == b; }
template <typename T>
bool GenericEqualityWithComparer(Gc<System::Collections::IEqualityComparer>,
                                 T a, T b) {
  return false;
}
template <typename T>
int GenericComparisonWithComparer(
    std::shared_ptr<System::Collections::IComparer> comp, T a, T b) {
  return 0;
}
template <typename T>
int GenericHashWithComparer(
    std::shared_ptr<System::Collections::IEqualityComparer> comp, T a) {
  return 0;
}
namespace IntrinsicFunctions {
template <typename T> T UnboxGeneric(std::shared_ptr<System::Object> obj) {
  return std::dynamic_pointer_cast<typename T::element_type>(obj);
//   return std::dynamic_pointer_cast<T>(obj);
}
} // namespace IntrinsicFunctions
} // namespace LanguagePrimitives
namespace Operators {
// int op_Addition(int x, int y) { return x + y; }
template <typename T> T op_LeftShift(T x, int n) { return x << n; }
template <typename T> T op_RightShift(T x, int n) { return x >> n; }
template <typename A, typename B, typename C> C op_Addition(A x, B y) {
  return x + y;
}
// std::string ToString(int x) { return std::string(""); }
std::string ToString(int x) { return std::to_string(x); }
template <typename T> std::string ToString(T x) { return x->ToString(); }
} // namespace Operators
} // namespace Core
} // namespace FSharp
} // namespace Microsoft