object Foo {

  def map[E](f: E |=> Int): (E |=> Int) = ???

  implicit def i: Int = ???

  def f: Int |=> Int = ???

  val a: Int = map(f)

}
