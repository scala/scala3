object Test extends App {
  class A {
    def run = "A"
  }
  object A
  class B[T] {
    def run = "B"
  }
  object B
  class C[S, T](x: S, y: T) {
    def run = s"C $x $y"
  }
  object C

  val x1 = A()
  assert(x1.run == "A")

  val x2 = B[String]()
  assert(x2.run == "B")

  val x3: B[String] = B()
  assert(x3.run == "B")

  val x4: C[String, Int] = C("a", 1)
  assert(x4.run == "C a 1")

  val x5 = C[String, Int]("a", 1)
  assert(x5.run == "C a 1")

  val x6 = C("a", 1)
  assert((x6: C[String, Int]).run == "C a 1")
/*
  val x7 = C[S = String]("a", 1)
  assert((x7: C[String, Int]).run == "C a 1")

  val x8 = C[T = Int]("a", 1)
  assert((x8: C[String, Int]).run == "C a 1")
*/
  Test2
}

object Test2 {
  class A {
    def run = "A"
  }
  class B[T] {
    def run = "B"
  }
  class C[S, T](x: S, y: T) {
    def run = s"C $x $y"
  }

  val x1 = Test.A()
  assert(x1.run == "A")

  val x2 = Test.B[String]()
  assert(x2.run == "B")

  val x3: B[String] = Test.B()
  assert(x3.run == "B")

  val x4: C[String, Int] = Test.C("a", 1)
  assert(x4.run == "C a 1")

  val x5 = Test.C[String, Int]("a", 1)
  assert(x5.run == "C a 1")

  val x6 = Test.C("a", 1)
  assert((x6: C[String, Int]).run == "C a 1")
}