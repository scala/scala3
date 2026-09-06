//> using options -language:experimental.inlineTraits
inline trait A:
  class Inner: // error: Inline traits may not define inner classes or traits.
    val x = 1

class B extends A:
  def f = Inner().x