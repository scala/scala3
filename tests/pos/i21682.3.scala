class Test:
  def foo[A1 >: (Nothing, Boolean, Nothing) <: (Any, Boolean, Any), B](f: A1 => B): Unit = ???
  def test(): Unit =
    val res4 = this.foo((b1: Boolean, b2: Boolean, b3: Boolean) => ???)
