class Tree
case class Inlined(call: Tree, bindings: List[String], expr: Tree) extends Tree
case object EmptyTree extends Tree
class Context

class Transform:
  def transform(tree: Tree)(using Context): Tree = tree

class Inliner:
  var enclosingInlineds: List[String] = Nil
  private def expandMacro(using Context) =
    val inlinedNormalizer = new Transform:
      override def transform(tree: Tree)(using Context) = tree match
        case Inlined(EmptyTree, Nil, expr) if enclosingInlineds.isEmpty => transform(expr)
        case _ => super.transform(tree)

object Inliner