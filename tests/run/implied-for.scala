trait T

object A {

  class B extends T
  class C extends T

  implied b for B
  implied c for C
  implied d for T
}

object Test extends App {
  import A._
  import implied A.{d, for B}

  println(d)
  val x: B = b

  assert(the[T].isInstanceOf[B])
}