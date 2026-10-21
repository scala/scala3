//> using options -language:experimental.inlineTraits
inline trait A:
  sealed class InnerA: // error: Inline traits may not define inner classes or traits.
    val x = 1

class B extends A:
  class InnerB extends InnerA:
    override val x = 2
  
def foo(x: A#InnerA) = println(x.x)

@main def main = 
  val a = new A() {}
  val inner_a = a.InnerA()
  foo(inner_a)

  val b = B()
  val inner_b = b.InnerB()
  foo(inner_b)
