trait Cap

trait Toolbox {
  type Tree

  val tpd: TypedTrees
  trait TypedTrees {
    type Tree
  }

  val Apply: ApplyImpl

  trait ApplyImpl {
    def unapply(tree: Tree): Option[(Tree, Seq[Tree])]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, Seq[tpd.Tree])]
  }
}

class Test(val tb: Toolbox) {
  import tb.*
  implicit val cap: Cap = null

  def foo(tree: Tree): Int = (tree: Any) match {
    case tb.Apply(fun, args) => 3  // error: ambiguous overload of unapply
  }

  def bar(tree: tpd.Tree): Int = (tree: Any) match {
    case Apply(fun, args) => 3  // error: ambiguous overload of unapply
  }
}
