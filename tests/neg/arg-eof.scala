object Test:
  case class Widget(name: String, other: Int = 5)
  Widget(name = "foo", // error