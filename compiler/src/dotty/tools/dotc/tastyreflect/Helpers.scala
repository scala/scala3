package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.Trees

trait Helpers {

  protected final def optional[T <: Trees.Tree[_]](tree: T): Option[tree.type] =
    if (tree.isEmpty) None else Some(tree)

}
