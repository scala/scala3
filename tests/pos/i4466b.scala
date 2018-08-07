object Foo {
  case class Bar(map: Map[String, String])

  object Bar {
    def apply(str: String): Bar = ???
  }

  Bar(Map("A" -> "B"))
}
