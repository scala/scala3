case class El[A](attr: String, child: String)

transparent inline def inlineTest(): String =
  inline {
    val el: El[Any] = El("1", "2")
    El[Any](el.attr, el.child)
  } match
    case El(attr, child) => attr + child

def test: Unit = inlineTest()
