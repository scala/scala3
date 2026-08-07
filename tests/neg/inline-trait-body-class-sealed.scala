//> using options -language:experimental.inlineTraits
inline trait A:
  sealed class InnerA: // error: Inline traits may not define inner classes or traits.
    val x = 1

class B extends A:
  class InnerB extends InnerA
  def f = InnerB().x