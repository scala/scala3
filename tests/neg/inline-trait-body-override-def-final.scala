//> using options -language:experimental.inlineTraits
inline trait A:
  final def f(x: Int) = x

class B extends A:
  override final def f(x: Int) = x + 1 // error