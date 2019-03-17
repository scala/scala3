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

  val x3: B[String] = Test.B()
  assert(x3.run == "B")

  val x4: C[String, Int] = C("a", 1)
  assert(x4.run == "C a 1")

  val x5 = C[String, Int]("a", 1)
  assert(x5.run == "C a 1")

  val x5a = C[S = String, T = Int]("a", 1)
  assert(x5a.run == "C a 1")

  val x5b = C[T = Int]("a", 1)
  assert(x5b.run == "C a 1")

  val x6 = C("a", 1)
  assert((x6: C[String, Int]).run == "C a 1")
  Test1
}

object Test1 {
  class A {
    def run = "A"
  }
  class B[T] {
    def run = "B"
  }
  class C[S, T](x: S, y: T) {
    def run = s"C $x $y"
  }

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
  Test2
}

object Test2 {
  val x1 = Test.A()
  assert(x1.run == "A")

  val x2 = Test.B[String]()
  assert(x2.run == "B")

  val x3: Test.B[String] = Test.B()
  assert(x3.run == "B")

  val x4: Test.C[String, Int] = Test.C("a", 1)
  assert(x4.run == "C a 1")

  val x5 = Test.C[String, Int]("a", 1)
  assert(x5.run == "C a 1")

  val x6 = Test.C("a", 1)
  assert((x6: Test.C[String, Int]).run == "C a 1")
  Test3
}

object Test3 {
  val x1 = Test1.A()
  assert(x1.run == "A")

  val x2 = Test1.B[String]()
  assert(x2.run == "B")

  val x3: Test1.B[String] = Test1.B()
  assert(x3.run == "B")

  val x4: Test1.C[String, Int] = Test1.C("a", 1)
  assert(x4.run == "C a 1")

  val x5 = Test1.C[String, Int]("a", 1)
  assert(x5.run == "C a 1")

  val x6 = Test1.C("a", 1)
  assert((x6: Test1.C[String, Int]).run == "C a 1")
  Test4
}

object Test4 {
  type A = Test.A
  type AA[T] = A
  type B[T] = Test.B[T]
  type C[T] = Test.C[T, Int]

  val x1 = A()
  assert(x1.run == "A")

  val x1a = AA[Int]()
  assert(x1a.run == "A")

  val x2 = B[String]()
  assert(x2.run == "B")

  val x3: B[String] = B()
  assert(x3.run == "B")

  val x5 = C[String]("a", 1)
  assert(x5.run == "C a 1")
  Test5
}

object Test5 {
  val x1 = Test4.A()
  assert(x1.run == "A")

  val x1a = Test4.AA[Int]()
  assert(x1a.run == "A")

  val x2 = Test4.B[String]()
  assert(x2.run == "B")

  val x3: Test4.B[String] = Test4.B()
  assert(x3.run == "B")

  val x5 = Test4.C[String]("a", 1)
  assert(x5.run == "C a 1")
  Test6
}

object Test6 {
  class A(s: String = "A") {
    def run = s
  }
  object A {
    def apply(): A = new A("X")
  }
  val x1 = A()
  assert(x1.run == "X")
}