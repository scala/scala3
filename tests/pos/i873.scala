object Test {
  def call(k: (Int, Int) => Unit): Unit = ???
  def test = call({ case (x, y) => ()})

  trait X extends Function1[Int, String]
  implicit def f2x(f: Function1[Int, String]): X = ???
  ({case _ if "".isEmpty => ""} : X) // allowed, implicit view used to adapt

  // ({case _ if "".isEmpty => 0} : X) // expected String, found Int
}
