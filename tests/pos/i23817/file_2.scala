object test {
  def t[T](expr: N[T]): Any =
    expr match {
      case MakeTuple(_) => ???
    }
}
