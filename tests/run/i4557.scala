trait F {
  def f: Unit
}

abstract class C0 extends F {
  def f = println("c0")
}

abstract class C1[A](val a: A) extends F {
  def f = {
    println("c1")
    println(a)
  }
}

abstract class C2[A, B](val a: A, val b: B) extends F {
  def f = {
    println("c2")
    println(a)
    println(b)
  }
}

object Test {
  type T0 = C0
  class DC0 extends C0
  class DT0 extends T0

  type T1[T] = C1[T]
  class DC1 extends C1[Int](0)
  class DT1 extends T1[Int](1)

  type T11 = C1[Int]
  class DT11 extends T11(2)

  type T12[X] = C1[X]
  class DT12 extends T12[Int](3)

  type T2[T, S] = C2[T, S]
  class DC2 extends C2[Int, String](0, "a")
  class DT2 extends T2[Int, String](1, "b")

  type T21[T] = C2[T, String]
  class DT21 extends T21[Int](2, "c")

  type T22 = C2[Int, String]
  class DT22 extends T22(3, "d")

  type T23[T, S] = T2[T, S]
  class DT23 extends T23(4, "e")

  type T24[X, Y] = C2[X, Y]
  class DT24 extends T24[Int, String](5, "f")

  def main(args: Array[String]) = {
    List(
      new DC0,
      new DT0,
      new DC1,
      new DT1,
      new DT11,
      new DT12,
      new DC2,
      new DT2,
      new DT21,
      new DT22,
      new DT23,
      new DT24
    ).foreach(_.f)
  }
}
