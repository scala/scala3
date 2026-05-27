object A {
  inline def foo = {
    val list = List[BigDecimal]()
    val bar = list.map(_.underlying)
  }
}
