object abc {
  trait Test0
  trait Test1 { def apply(f: Int => Int): Unit }

  def v: Test0 = ???
  def v[T]: Test1 = ???
  //def v[T](x: String): Test0 = ???

  v[Int]{ v => v }
}