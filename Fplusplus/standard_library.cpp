#include <concepts>
#define GC_THREADS

#include <any>
#include <iostream>
#include <print>
// #include <memory>
#include <functional>
#include <string>
#include <type_traits>
#include <vector>

// clang-format off
#include <gc/gc_cpp.h>
#include <gc/gc_allocator.h>
// clang-format on

static std::ios_base::Init __stream_initializer;

#ifndef INITMACRO
#define INITMACRO
#define CONCAT_HIDDEN(a, b) a ## b
#define CONCAT(a, b) CONCAT_HIDDEN(a, b)
#define INIT auto CONCAT(__, __COUNTER__) = [] {
#define ENDINIT return 0; }();
#endif

template <typename T>
class Ref {
public:
  T* Value;
  Ref(const T& value) {
    Value = (T*)GC_malloc(sizeof(T));
    // *Value = value;
    new (Value) T(value);
  }
  Ref(T* value) : Value(value) {}
  // Ref(const Ref&) = delete;
  Ref(const Ref& other) : Value(other.Value) {};
  operator T&() { return *Value; }
  operator const T&() const { return *Value; }
  T* operator->() { return Value; }
  const T* operator->() const { return Value; }
  Ref& operator=(const Ref&) = delete;


  Ref& operator=(const T& value) {
    *Value = value;
    return *this;
  }
};


struct SetupBoehmGC {
public:
  SetupBoehmGC() { GC_INIT(); }
  ~SetupBoehmGC() { GC_gcollect(); }
};

SetupBoehmGC __setupBoehmGc;

template <typename T> using Gc = T *;

template <typename From, typename To, typename T>
To coerce(T& value) { return dynamic_cast<To>(static_cast<From>(value)); }

namespace System {
class Object;
}

#define gcnew new (UseGC)
std::vector<System::Object **> objectRoots;

template <typename T>
  requires std::derived_from<std::remove_pointer_t<T>, System::Object>
class GcRoot {
public:
  T data;
  GcRoot() : data(nullptr) {}
  GcRoot(T object) : data(object) {
    objectRoots.push_back(reinterpret_cast<System::Object **>(&data));
  }
  ~GcRoot() { objectRoots.pop_back(); }
  T operator->() { return data; }
  operator T() { return data; }
  T get() { return data; }
  GcRoot(const GcRoot &) = delete;
//   GcRoot &operator=(const GcRoot &) = delete;
};

template <typename T>
  requires std::derived_from<std::remove_pointer_t<T>, System::Object>
class TempRef {
public:
  T data;
  TempRef(T object) : data(object) {}
  T operator->() { return data; }
  operator T() { return data; }
  T get() { return data; }
};

namespace System {
class Object {

public:
  virtual ~Object() = default;
  virtual std::string ToString() { return "System.Object"; }

public:
  std::any __data;
};
template <typename T> bool IsType(Gc<System::Object> obj) {
  if constexpr (std::is_pointer_v<T>) {
    return dynamic_cast<T>(obj) != nullptr;
  } else {
    return obj->__data.type() == typeid(T);
  }
}
}

template <typename A, typename B>
class FSharpFunc : public System::Object {
std::function<B(A)> fn;
public:
  FSharpFunc(std::function<B(A)> fn) : fn(fn) {}
  FSharpFunc* operator->() { return this; }

  B operator()(A a) { return fn(a); }
  B invoke(A a) { return fn(a); }
  operator std::function<B(A)>&() { return *fn; }
};
template <typename B>
class FSharpFunc<void, B> : public System::Object {
std::function<B()> fn;
public:
  FSharpFunc(std::function<B()> fn) : fn(fn) {}
  FSharpFunc* operator->() { return this; }

  // B operator()(A a) { return fn(a); }
  B invoke() { return fn(); }
  operator std::function<B()>&() { return fn; }
};


namespace System {
template <typename T>
class Box : public virtual Object {
T data;
public:
  Box(T value) : data(value) {}
  T* get() { return &data; }
};

class String : public Object {
public:
  char *data;
  // std::string data;
  String() : data(nullptr) {}
  String(const char *chars) {
    auto length = strlen(chars) + 1;
    data = (char *)GC_malloc_atomic(length);
    memcpy(data, chars, length);
  }
  String(const std::string& str) : String(str.c_str()) {
  }

  operator std::string() { return std::string(data); }

  bool operator==(const String &other) const {
    return strcmp(data, other.data) == 0;
  }
};

template <typename T> class IComparable_1 {};

class IComparable {};

class ValueType : public Object {};

template <typename T> class IEquatable_1 {};

class IDisposable {
public:
  virtual ~IDisposable() = default;
  void Dispose() { this->System_IDisposable_Dispose(); }

  virtual void System_IDisposable_Dispose() = 0;
};

namespace Collections {
class IEnumerator : public IDisposable {
public:
  virtual IEnumerator GetEnumerator() = 0;
};

template <typename T> class IEnumerator_1 : public IDisposable {
public:
  IEnumerator_1<T> *GetEnumerator();
  bool MoveNext() { return this->System_Collections_IEnumerator_1_MoveNext(); }

  T get_Current() {
    return this->System_Collections_IEnumerator_1_get_Current();
  }

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
template <typename T>
class List_1 : public virtual System::Object, public IEnumerable_1<T> {
  std::vector<T, gc_allocator<T>> items;

public:
  class Enumerator : public IEnumerator_1<T> {
    int index = 0;
    T current;
    List_1<T> *list;

  public:
    Enumerator(List_1<T> *xs) : list(xs) {}

    bool System_Collections_IEnumerator_1_MoveNext() override {
      if (index < list->items.size()) {
        current = list->items[index];
        index++;
        return true;
      }
      index = -1;
      return false;
    }

    T System_Collections_IEnumerator_1_get_Current() override {
      return current;
    }

    void System_Collections_IEnumerator_1_Reset() override { index = 0; }

    void System_IDisposable_Dispose() override {}
  };

  void Add(T value) { items.push_back(value); }
  List_1::Enumerator GetEnumerator() { return Enumerator(this); }
};
} // namespace Generic
} // namespace Collections
namespace Console {
void WriteLine(const System::String *s) { std::cout << s->data << std::endl; }
void WriteLine(const System::String &s) { std::cout << s.data << std::endl; }
void WriteLine(const System::Object &o) { std::println("System.Object"); }
void WriteLine(std::string s) { std::cout << s << std::endl; }
void WriteLine(int n) { std::cout << n << std::endl; }
} // namespace Console
} // namespace System
namespace Microsoft {
namespace FSharp {
namespace Collections {
// template <typename T> class ResizeArray_1 {};
template <typename T>
using ResizeArray_1 = System::Collections::Generic::List_1<T>;

template <typename T> class list_1 : public virtual System::Object {};

template <typename T> class seq_1 : public virtual System::Object {};

namespace SeqModule {
template <typename T> list_1<T> *ToList(seq_1<T> *xs);
template <typename T, typename U>
seq_1<U> *Map(std::function<U(T)> fn, seq_1<T> *xs);
template <typename T> seq_1<T> *Delay(std::function<seq_1<T> *()> toDelay);
} // namespace SeqModule
} // namespace Collections
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
int GenericComparisonWithComparer(Gc<System::Collections::IComparer> comp, T a,
                                  T b) {
  return 0;
}

template <typename T>
int GenericHashWithComparer(Gc<System::Collections::IEqualityComparer> comp,
                            T a) {
  return 0;
}

namespace IntrinsicFunctions {
template <typename T> T UnboxGeneric(Gc<System::Object> obj) {
  // return std::dynamic_pointer_cast<typename T::element_type>(obj);
  if constexpr (std::is_pointer_v<T>) {
    return dynamic_cast<T>(obj);
  } else {
    return std::any_cast<T>(obj->__data);
  }
}
} // namespace IntrinsicFunctions
} // namespace LanguagePrimitives
namespace Operators {

template <typename T> T op_LeftShift(T x, int n) { return x << n; }

template <typename T> T op_RightShift(T x, int n) { return x >> n; }

template <typename A, typename B, typename C> C op_Addition(A x, B y) {
  return x + y;
}

template <typename A, typename B, typename C> C op_Multiply(A x, B y) {
  return x * y;
}

template <typename T> Collections::seq_1<T> *op_Range(T start, T end);

template <typename A, typename B> bool op_Inequality(A a, B b) {
  return a == b;
}

template <typename A, typename B> bool op_LessThan(A x, B y) { return x < y; }

template <typename A, typename B> bool op_GreaterThan(A x, B y) {
  return x > y;
}

template <typename T>
Collections::seq_1<T> *CreateSequence(Collections::seq_1<T> *xs) {
  return xs;
}

// std::string ToString(int x) { return std::to_string(x); }

System::String ptr_to_string(System::Object* x) {
  return x->ToString();
}
template <typename T> System::String ToString(T x) {
  if constexpr (std::is_pointer_v<T>) {
    return ptr_to_string(x);
  } else if constexpr (std::is_arithmetic_v<T>) {
    // For int, double, float, etc.
    return std::to_string(x);
  } else {
    return x.ToString();
  }
}

template <typename T> std::string ToString(Gc<T> x) { return x->ToString(); }
} // namespace Operators
} // namespace Core
} // namespace FSharp
} // namespace Microsoft

using namespace System;
using namespace Microsoft::FSharp::Collections;
using namespace Microsoft::FSharp::Core::Operators;
using namespace Microsoft::FSharp::Core::LanguagePrimitives;
using namespace Microsoft::FSharp::Core::LanguagePrimitives::IntrinsicFunctions;