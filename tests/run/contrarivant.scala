object Test extends App {

  implicit def f: String => String = x => "f"

  implicit def g: Object => String = x => "g"

  def h(implicit x: String => String) = x("")

  implicit def fs: List[String => String] = List(f)

  implicit def gs: List[Object => String] = List(g)

  def hs(implicit xs: List[String => String]) = xs.head("")

  assert(h == "f")
  assert(hs == "f")
}
