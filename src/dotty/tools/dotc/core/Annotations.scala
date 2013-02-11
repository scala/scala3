package dotty.tools.dotc.core

import Symbols._, Trees._, Types._, Positions._, Contexts._

object Annotations {

  abstract class Annotation {
    def tree: TypedTree
    def symbol(implicit ctx: Context): Symbol = tree.tpe.typeSymbol
    def matches(cls: Symbol)(implicit ctx: Context): Boolean = symbol.isNonBottomSubClass(cls)
    def appliesToModule: Boolean = ???
  }

  case class ConcreteAnnotation(val tree: TypedTree) extends Annotation

  object Annotation {

    def apply(cls: ClassSymbol, args: List[TypedTree])(implicit ctx: Context): Annotation =
      ConcreteAnnotation(makeTypedTree.New(cls.typeConstructor, args))

    def makeAlias(sym: TermSymbol)(implicit ctx: Context) =
      apply(defn.AliasAnnot, List(makeTypedTree.Ident(TermRef(sym.owner.thisType, sym))))

    def makeChild(sym: ClassSymbol)(implicit ctx: Context) =
      apply(defn.ChildAnnot, List(makeTypedTree.Ident(TypeRef(sym.owner.thisType, sym))))

  }
}