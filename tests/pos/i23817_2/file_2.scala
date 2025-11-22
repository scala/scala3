object test {
  def f[T](c: TC1[T]): Unit =
    c match {
      case Iterable(_) => ???
    }
}
