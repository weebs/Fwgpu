#include <iostream>
#include <any>
// #include <memory>
#include <string>
static std::ios_base::Init stream_initializer;
// template <typename T> using Gc = std::shared_ptr<T>;
template <typename T> using Gc = T *;
template <typename T> class GcRoot {
public:
  T data;
  GcRoot(T object) : data(object) {}
  T operator->() { return data; }
  operator T() { return data; }
};
namespace System {
class Object {
  //   std::string type;

public:
  virtual ~Object() = default;
  //   bool IsType(std::string typeName) { return type == typeName; }
  virtual std::string ToString() { return "System.Object"; }

public:
  // void *__data; // TODO : deleting in the destructor
  std::any __data;
};
template <typename T> bool IsType(Gc<System::Object> obj) {
  // return std::dynamic_pointer_cast<T>(obj) != nullptr;
  // return dynamic_cast<T>(obj) != nullptr;
  // return std::any_cast<T> != nullptr;
  return obj->__data.type() == typeid(T);
}
template <typename T> class IComparable_1 {};
class IComparable {};
class ValueType {};
template <typename T> class IEquatable_1 {};
class IDisposable {
public:
  virtual ~IDisposable() = default;
  void Dispose() { this->System_IDisposable_Dispose(); }
  virtual void System_IDisposable_Dispose() {};
};
namespace Collections {
class IEnumerator : public IDisposable {
public:
  virtual IEnumerator GetEnumerator() = 0;
};
template <typename T> class IEnumerator_1 : public IDisposable {
public:
  IEnumerator_1<T>* GetEnumerator();
  bool MoveNext() { return this->System_Collections_IEnumerator_1_MoveNext(); }
  T get_Current() { return this->System_Collections_IEnumerator_1_get_Current(); }
  void Reset() { this->System_Collections_IEnumerator_1_Reset(); }
  virtual bool System_Collections_IEnumerator_1_MoveNext() = 0;
  virtual T System_Collections_IEnumerator_1_get_Current() = 0;
  virtual void System_Collections_IEnumerator_1_Reset() = 0;
};
class IEnumerable {};
template <typename T> class IEnumerable_1 : public IEnumerable {};
class IStructuralEquatable {};
class IStructuralComparable {};
class IComparer {};
class IEqualityComparer {};
namespace Generic {
template <typename T> class List_1 : IEnumerable_1<T> {
  std::vector<T> items;
public:
  class Enumerator : public IEnumerator_1<T> {
    int index = 0;
    T current;
    List_1<T>* list;
  public:
    Enumerator(List_1<T>* xs) : list(xs) {}
    bool System_Collections_IEnumerator_1_MoveNext() override {
      if (index < list->items.size()) {
        current = list->items[index];
        index++; return true;
      }
      index = -1;
      return false;
    }
    T System_Collections_IEnumerator_1_get_Current() override {
      return current;
    }
    void System_Collections_IEnumerator_1_Reset() override {
      index = 0;
    }
  };
  void Add(T value)
  {
    items.push_back(value);
  }
  List_1::Enumerator GetEnumerator() {
    return Enumerator(this);
  }
};
} // namespace Generic
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
bool GenericEqualityWithComparer(Gc<System::Collections::IEqualityComparer>, T a,
                                 T b) {
  return false;
}
template <typename T>
int GenericComparisonWithComparer(Gc<System::Collections::IComparer> comp, T a,
                                  T b) {
  return 0;
}
template <typename T>
int GenericHashWithComparer(Gc<System::Collections::IEqualityComparer> comp, T a) {
  return 0;
}
namespace IntrinsicFunctions {
template <typename T> T UnboxGeneric(Gc<System::Object> obj) {
  // return std::dynamic_pointer_cast<typename T::element_type>(obj);
  // if (dynamic_cast<T*>(obj->__data) != nullptr) {
  //   return *dynamic_cast<T*>(obj->__data);
  // }
  // if (std::any_cast<T>(obj->__data) != nullptr) {
  //   return std::any_cast<T>(obj->__data);
  // }
  // if (obj->__data.type() == typeid(T)) {
    return std::any_cast<T>(obj->__data);
  // }
  // return dynamic_cast<T>(obj);
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
template <typename A, typename B, typename C> C op_Multiply(A x, B y) {
  return x * y;
}
// std::string ToString(int x) { return std::string(""); }
std::string ToString(int x) { return std::to_string(x); }
template <typename T> std::string ToString(T x) { return x->ToString(); }
template <typename T> std::string ToString(Gc<T> x) { return x->ToString(); }
} // namespace Operators
} // namespace Core
namespace Collections {
// template <typename T> class ResizeArray_1 {};
template <typename T>
using ResizeArray_1 = System::Collections::Generic::List_1<T>;
} // namespace Collections
} // namespace FSharp
} // namespace Microsoft