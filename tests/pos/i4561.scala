object abc:
  trait Test0
  trait Test1[T]:
    def apply(f: T => T): Unit
    def apply(s: String): Unit

  def v: Test0 = ???
  def v[T]: Test1[Int] = ???

  def w: Test0 = ???
  def w[T]: Test1[T] = ???
  def w[T](x: String): Test1[T] = ???

  v[Any](x => x)
  val v1: Test0 = v
  v(x => x + 1)

  w[Int](_ + 1)
  // w(_ + 1)      // error: `+` is not a member of Any
  w[Int]("x")(_ + 1)
