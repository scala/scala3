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
  import tb._
  implicit val cap: Cap = null

  def foo(tree: Tree): Int = tree match { // error: private escape
    case tb.Apply(fun, args) => 3 // error
  }

  def bar(tree: tpd.Tree): Int = tree match {  // error: private escape
    case Apply(fun, args) => 3 // error
  }
}
