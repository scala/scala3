object Foo {

  def map[E](f: implicit E => Int): (implicit E => Int) = ???

  implicit def i: Int = ???

  def f: implicit Int => Int = ???

  val a: Int = map(f)

}
