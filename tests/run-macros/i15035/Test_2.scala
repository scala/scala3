trait A {
  val v1: Int
  lazy val v2: Int
  def f1(): Int
  def f2(foo: String): Int
  def f3: Int
}

trait B {
  val v1: Int = 2
  lazy val v2: Int = 2
  def f1(): Int = 2
  def f2(foo: String): Int = 2
  def f3: Int = 2
}


@main
def Test = {
  val a: A = make[A](1)
  println(a.v1)
  println(a.v2)
  println(a.f1())
  println(a.f2("test"))
  println(a.f3)

  val b: B = make[B](3)
  println(b.v1)
  println(b.v2)
  println(b.f1())
  println(b.f2("test"))
  println(b.f3)
}
