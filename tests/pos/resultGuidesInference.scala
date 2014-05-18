object test {

  def foo[T](x: T => T): Array[T] = ???

  val x: Array[Int] = foo(x => x)

}
