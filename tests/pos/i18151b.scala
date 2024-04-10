case class El[A](val attr: String, val child: String)

transparent inline def tmplStr(inline t: El[Any]): String =
  inline t match
    case El(attr, child) => attr + child

def test: Unit = tmplStr {
  val el = El("1", "2")
  El[Any](el.attr, null)
}
