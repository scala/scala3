package dotty.tools.dotc.core

import Symbols._, Types._, Positions._, Contexts._, Constants._, TypedTrees.tpd._

object Annotations {

  abstract class Annotation {
    def tree: Tree
    def symbol(implicit ctx: Context): Symbol = tree.tpe.typeSymbol
    def matches(cls: Symbol)(implicit ctx: Context): Boolean = symbol.isNonBottomSubClass(cls)
    def appliesToModule: Boolean = ???
  }

  case class ConcreteAnnotation(val tree: Tree) extends Annotation

  object Annotation {

    def apply(tree: Tree) = ConcreteAnnotation(tree)

    def apply(cls: ClassSymbol, arg: Tree)(implicit ctx: Context): Annotation =
      apply(cls, arg :: Nil)

    def apply(cls: ClassSymbol, arg1: Tree, arg2: Tree)(implicit ctx: Context): Annotation =
      apply(cls, arg1 :: arg2 :: Nil)

    def apply(cls: ClassSymbol, args: List[Tree])(implicit ctx: Context): Annotation =
      apply(cls.typeConstructor, args)

    def apply(atp: Type, arg: Tree)(implicit ctx: Context): Annotation =
      apply(atp, arg :: Nil)

    def apply(atp: Type, arg1: Tree, arg2: Tree)(implicit ctx: Context): Annotation =
      apply(atp, arg1 :: arg2 :: Nil)

    def apply(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation =
      apply(New(atp, args))

    def makeAlias(sym: TermSymbol)(implicit ctx: Context) =
      apply(defn.AliasAnnot, List(Ident(TermRef(sym.owner.thisType, sym.name, sym.signature))))

    def makeChild(sym: Symbol)(implicit ctx: Context) =
      apply(defn.ChildAnnot, List(Ident(NamedType(sym.owner.thisType, sym.name))))
  }

  def makeLiteralAnnotArg(const: Constant): Tree = ???

  def makeArrayAnnotArg(elems: Array[Tree]): Tree = ???

  def makeNestedAnnotArg(annot: Annotation): Tree = annot.tree

  def ThrowsAnnotation(cls: ClassSymbol)(implicit ctx: Context) =
    Annotation(defn.ThrowsAnnot, Ident(cls.symbolicRef))
}