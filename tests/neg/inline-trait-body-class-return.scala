inline trait A:
  sealed class InnerA: // error: Inline traits may not define inner classes or traits.
    val x = 1
  def generate(x: Int) = InnerA()

class B extends A:
  val y = generate(7)
