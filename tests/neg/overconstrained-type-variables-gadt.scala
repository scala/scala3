object Test {
  trait T1[A] { def a: A }
  trait T2[B] { def b: B }

  def foo[X, Y](v: T1[X] | T2[Y]): X = v match {
    case t1: T1[t] =>
      // consider: foo[Int, String](new T1[String] with T2[String] { ... })
      t1.a // error
  }

  class T1Int extends T1[Int] { def a = 0 }
  def bar[X, Y](v: T1[X] | T2[Y]): X = v match {
    case t1: T1Int =>
      // similar reasoning to above applies
      t1.a // error
  }

  class T1IntT2String extends T1[Int] with T2[String] {
    def a = 0
    def b = ""
  }
  def baz[X](v: T1[X] | T2[X]): Unit = v match {
    case _: T1IntT2String =>
      val x1: X = 0 // error
      val x2: X = "" // error
  }
}
