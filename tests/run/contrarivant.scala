object Test extends App {

  // toplevel contra
  implicit def f: String => String = x => "f"
  implicit def g: Object => String = x => "g"
  def h(implicit x: String => String) = x("")
  assert(h == "f", h)

  // nested contra
  implicit def fs: List[String => String] = List(f)
  implicit def gs: List[Object => String] = List(g)
  def hs(implicit xs: List[String => String]) = xs.head("")
  assert(hs == "f", hs)

  // covariant subapplication nested in toplevel contra
  implicit def f2: (Unit => String) => String = x => "f2"
  implicit def g2: (Unit => Object) => String = x => "g2"
  def h2(implicit x: (Unit => String) => String) = x(_ => "")
  assert(h2 == "f2", h2)

  // covariant subapplication nested in nested contra
  implicit def fs2: List[(Unit => String) => String] = List(f2)
  implicit def gs2: List[(Unit => Object) => String] = List(g2)
  def hs2(implicit xs: List[(Unit => String) => String]) = xs.head(_ => "")
  assert(hs2 == "f2", hs2)
}
