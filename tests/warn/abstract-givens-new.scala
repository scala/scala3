//> using options -language:experimental.modularity -source future
class C:
  type Self

trait T:
  given Int is C // ok
  given intC: Int is C // warn
  given intC2: (Int is C)() // ok
  given intC3: Int is C {} // also ok

