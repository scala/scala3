def main = {
  ("foo": String) match {
    case a: (String | Nothing) => // error
      println("bar")
    case _ =>
      println("foo")
  }
}
