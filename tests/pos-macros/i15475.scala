def test =
  transform {
    val a: Seq[Generic[?]] = ???
    a.foreach { to =>
      to.mthd()
    }
  }

transparent inline def transform[T](expr: T): T = ???

trait Generic[+T] {
  def mthd(): Generic[T] = ???
}
