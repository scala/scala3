sealed trait CL3Literal
case object IntLit extends CL3Literal
case object CharLit extends CL3Literal
case object BooleanLit extends CL3Literal
case object UnitLit extends CL3Literal


sealed trait Tree
case class LetL(value: CL3Literal) extends Tree
case object LetP extends Tree
case object LetC extends Tree
case object LetF extends Tree

object Test {
  def transform(tree: Tree) : Any = tree match {
    case LetL(CharLit) =>
      ???
  }
}
