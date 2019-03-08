object Foo {

  def map[E](f: given E => Int): (given E => Int) = ???

  implicit def i: Int = ???

  def f: given Int => Int = ???

  val a: Int = map(f)

}
