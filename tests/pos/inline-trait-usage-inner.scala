//> using options -language:experimental.inlineTraits
object O:
  inline trait A[T]:
    def t: T = ???

class B extends O.A[Int]
