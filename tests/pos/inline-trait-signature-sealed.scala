//> using options -language:experimental.inlineTraits
inline sealed trait A:
  final def f(x: Int) = x

class B extends A