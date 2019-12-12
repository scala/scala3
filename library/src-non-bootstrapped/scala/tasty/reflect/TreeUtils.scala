package scala.tasty
package reflect

/** Tasty reflect case definition */
trait TreeUtils
    extends Core
    with SymbolOps
    with TreeOps { self: Reflection =>

  abstract class TreeAccumulator[X] {
    def foldTree(x: X, tree: Tree)(given ctx: Context): X
    def foldTrees(x: X, trees: Iterable[Tree])(given ctx: Context): X =
      throw new Exception("non-bootstraped-library")
    def foldOverTree(x: X, tree: Tree)(given ctx: Context): X =
      throw new Exception("non-bootstraped-library")
  }

  abstract class TreeTraverser extends TreeAccumulator[Unit] {
    def traverseTree(tree: Tree)(given ctx: Context): Unit =
      throw new Exception("non-bootstraped-library")
    def foldTree(x: Unit, tree: Tree)(given ctx: Context): Unit =
      throw new Exception("non-bootstraped-library")
    protected def traverseTreeChildren(tree: Tree)(given ctx: Context): Unit =
      throw new Exception("non-bootstraped-library")
  }

  abstract class TreeMap { self =>
    def transformTree(tree: Tree)(given ctx: Context): Tree =
      throw new Exception("non-bootstraped-library")
    def transformStatement(tree: Statement)(given ctx: Context): Statement =
      throw new Exception("non-bootstraped-library")
    def transformTerm(tree: Term)(given ctx: Context): Term =
      throw new Exception("non-bootstraped-library")
    def transformTypeTree(tree: TypeTree)(given ctx: Context): TypeTree =
      throw new Exception("non-bootstraped-library")
    def transformCaseDef(tree: CaseDef)(given ctx: Context): CaseDef =
      throw new Exception("non-bootstraped-library")
    def transformTypeCaseDef(tree: TypeCaseDef)(given ctx: Context): TypeCaseDef =
      throw new Exception("non-bootstraped-library")
    def transformStats(trees: List[Statement])(given ctx: Context): List[Statement] =
      throw new Exception("non-bootstraped-library")
    def transformTrees(trees: List[Tree])(given ctx: Context): List[Tree] =
      throw new Exception("non-bootstraped-library")
    def transformTerms(trees: List[Term])(given ctx: Context): List[Term] =
      throw new Exception("non-bootstraped-library")
    def transformTypeTrees(trees: List[TypeTree])(given ctx: Context): List[TypeTree] =
      throw new Exception("non-bootstraped-library")
    def transformCaseDefs(trees: List[CaseDef])(given ctx: Context): List[CaseDef] =
      throw new Exception("non-bootstraped-library")
    def transformTypeCaseDefs(trees: List[TypeCaseDef])(given ctx: Context): List[TypeCaseDef] =
      throw new Exception("non-bootstraped-library")
    def transformSubTrees[Tr <: Tree](trees: List[Tr])(given ctx: Context): List[Tr] =
      throw new Exception("non-bootstraped-library")
  }

}
