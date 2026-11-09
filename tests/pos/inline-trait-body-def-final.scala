//> using options -language:experimental.inlineTraits
inline trait A:
  final def f(x: Int) = x

class B extends A