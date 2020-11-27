case class Foo(s: String) {
  def appliedType(tycon: Any) =
    tycon match {
        case Foo(("NothingClass" | "AnyClass") as sym) => println(sym)
    }
}
