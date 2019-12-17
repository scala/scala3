class A
class B
class C
class D
class E
trait F

given A()
given B()
given (A, B) => C()    // this one is equivalent to ...
given A, B => D(), F // ... the one without the parens
given ((A, B)) => E()  // to demand a tuple, add an extra pair of parens

@main def Test =
  summon[C]
  summon[D]
  summon[F]
  given (A, B) = (summon[A], summon[B])
  summon[E]
