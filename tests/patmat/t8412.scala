sealed trait Tree
case class Let(sth: List[Any]) extends Tree
case class Lit(sth: Any) extends Tree

object Test {
  def wroong(tree: Tree) =
    tree match {
      case Let(_ :: rest) =>
        ???
      case Let(Nil) =>
        ???
      // no warning for missing Lit(_) in 2.10
    }
}
