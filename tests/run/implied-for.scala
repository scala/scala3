trait T

object A {

  class B extends T
  class C extends T
  class D[T]

  implied b for B
  implied c for C
  implied t for T
  implied d for D[Int]
}

object Test extends App {
  import A._
  import implied A.{t, for B, D[_]}

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
  implied intOrd for Ordering[Int]
  implied listOrd[T] for Ordering[List[T]] given Ordering[T]
  implied ec for ExecutionContext
  implied im for Monoid[Int]
}

object Test2 {
  import implied Instances.{for Ordering[_], ExecutionContext}
  val x = intOrd
  val y = listOrd[Int]
  val z = ec
  the[Ordering[Int]]
  the[Ordering[List[Int]]]
  the[ExecutionContext]
}

object Test3 {
  import implied Instances.{im, for Ordering[_]}
  val x = intOrd
  val y = listOrd[Int]
  val z = im
  the[Ordering[Int]]
  the[Ordering[List[Int]]]
  the[Monoid[Int]]
}

