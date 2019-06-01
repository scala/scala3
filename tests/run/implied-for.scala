trait T

object A {

  class B extends T
  class C extends T
  class D

  implied b for B
  implied c for C
  implied t for T
  implied d for D
}

object Test extends App {
  import A._
  import implied A.{t, for B, D}

  val x1: B = b
  val x2: T = t
  val x3: D = d

  assert(the[T].isInstanceOf[B])
  assert(the[D].isInstanceOf[D])
}