import scala.quoted.*

inline def myTreeTraverse[T](inline expr: T): T = ${ treeTraverseImpl('expr) }

def treeTraverseImpl[T: Type](value: Expr[T])(using quotes: Quotes): Expr[T] = {
  import quotes.reflect.*
  (new TreeTraverser {}).traverseTree(value.asTerm)(Symbol.spliceOwner)
  value
}
