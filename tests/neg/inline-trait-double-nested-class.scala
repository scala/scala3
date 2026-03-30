inline trait A:
  sealed class InnerA: // error: Inline traits may not define inner classes or traits.
      sealed class InnerInnerA:
        val x = 1

class B extends A:
  class InnerB extends InnerA {
    class InnerInnerB extends InnerInnerA
  }
  def f =
    val a = new InnerB()
    val b = a.InnerInnerB()
    b.x
