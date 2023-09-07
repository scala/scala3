class Pet
class Cat extends Pet

class Z1[ S1 <: Pet](val fn: S1 => Unit)
class Z2[ S2       ](val fn: S2 => Unit)
class Z3[-S3 <: Pet](val fn: S3 => Unit)

abstract class Test:
  def test =
    val r1 = new Z1((_: Pet) => ()); eat[Z1[Pet]](r1) // the case: using the parameter bound in situ infers Z[Nothing]
    val r2 = new Z2((_: Pet) => ()); eat[Z2[Pet]](r2) // counter-example: infers as desired without an upper bound
    val r3 = new Z3((_: Pet) => ()); eat[Z3[Pet]](r3) // workaround: declare it contravariant
    val r4 = new Z1((_: Cat) => ()); eat[Z1[Cat]](r4) // counter-example: infers as desired with a subtype

  def eat[T](x: T): Unit
