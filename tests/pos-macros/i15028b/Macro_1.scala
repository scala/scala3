import scala.quoted.*

inline def myTreeTraverse[T](inline expr: T): T = ${ treeTraverseImpl('expr) }

def treeTraverseImpl[T: Type](value: Expr[T])(using quotes: Quotes): Expr[T] = {
  import quotes.reflect.*
  (new TreeTraverser {
  override def traverseTree(tree: Tree)(owner: Symbol): Unit =
    tree.show
    tree.show(using Printer.TreeStructure)
    super.traverseTree(tree)(owner)
  }).traverseTree(value.asTerm)(Symbol.spliceOwner)
  value
}
