object Foo {
  def bar = {
    val data = Map.empty[String, String]
    val list = List.empty[Map[String, String]]
    list.map(_ ++ data)
  }
}
