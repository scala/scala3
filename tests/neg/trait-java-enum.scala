trait T extends java.lang.Enum[T]

enum MyEnum extends T { // error: enum class MyEnum extends java.lang.Enum[T], but the type argument must be the enum class itself
  case A, B
}