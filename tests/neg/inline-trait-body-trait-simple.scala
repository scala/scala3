//> using options -language:experimental.inlineTraits
inline trait A[T]:
  trait InnerA: // error: Inline traits may not contain inner classes.
    def x: T = ???

class B extends A[Int]:
  class InnerB extends InnerA