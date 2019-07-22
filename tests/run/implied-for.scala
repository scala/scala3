trait T

object A {

  class B extends T
  class C extends T
  class D[T]

  given b as B
  given c as C
  given t as T
  given d as D[Int]
}

object Test extends App {
  import A._
  import given A.{t, for B, D[_]}

  val x1: B = b
  val x2: T = t
  val x3: D[Int] = d

  assert(the[T].isInstanceOf[B])
  assert(the[D[Int]].isInstanceOf[D[_]])
}

class Ordering[T]
class ExecutionContext
class Monoid[T]

object Instances {
  given intOrd as Ordering[Int]
  given listOrd[T] as Ordering[List[T]] given Ordering[T]
  given ec as ExecutionContext
  given im as Monoid[Int]
}

object Test2 {
  import given Instances.{for Ordering[_], ExecutionContext}
  val x = intOrd
  val y = listOrd[Int]
  val z = ec
  the[Ordering[Int]]
  the[Ordering[List[Int]]]
  the[ExecutionContext]
}

object Test3 {
  import given Instances.{im, for Ordering[_]}
  val x = intOrd
  val y = listOrd[Int]
  val z = im
  the[Ordering[Int]]
  the[Ordering[List[Int]]]
  the[Monoid[Int]]
}

