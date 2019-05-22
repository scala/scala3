object Test {
  trait T1[A] { def a: A }
  trait T2[B] { def b: B }

  def foo[X, Y](u: T1[X] | T2[Y]): X = u match {
    case t1: T1[t] =>
      // consider: foo[Int, String](new T1[String] with T2[String] { ... })
      t1.a // error
  }

  class T1Int extends T1[Int] { def a = 0 }
  def bar[X, Y](u: T1[X] | T2[Y]): X = u match {
    case t1: T1Int =>
      // similar reasoning to above applies
      t1.a // error
  }
}
