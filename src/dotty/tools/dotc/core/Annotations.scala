package dotty.tools.dotc.core

import Symbols._, Trees._, Types._, Positions._, Contexts._, Constants._

object Annotations {

  abstract class Annotation {
    def tree: TypedTree
    def symbol(implicit ctx: Context): Symbol = tree.tpe.typeSymbol
    def matches(cls: Symbol)(implicit ctx: Context): Boolean = symbol.isNonBottomSubClass(cls)
    def appliesToModule: Boolean = ???
  }

  case class ConcreteAnnotation(val tree: TypedTree) extends Annotation

  object Annotation {

    def apply(cls: ClassSymbol, arg: TypedTree)(implicit ctx: Context): Annotation =
      apply(cls, arg :: Nil)

    def apply(cls: ClassSymbol, arg1: TypedTree, arg2: TypedTree)(implicit ctx: Context): Annotation =
      apply(cls, arg1 :: arg2 :: Nil)

    def apply(cls: ClassSymbol, args: List[TypedTree])(implicit ctx: Context): Annotation =
      apply(cls.typeConstructor, args)

    def apply(atp: Type, arg: TypedTree)(implicit ctx: Context): Annotation =
      apply(atp, arg :: Nil)

    def apply(atp: Type, arg1: TypedTree, arg2: TypedTree)(implicit ctx: Context): Annotation =
      apply(atp, arg1 :: arg2 :: Nil)

    def apply(atp: Type, args: List[TypedTree])(implicit ctx: Context): Annotation =
      ConcreteAnnotation(makeTypedTree.New(atp, args))

    def makeAlias(sym: TermSymbol)(implicit ctx: Context) =
      apply(defn.AliasAnnot, List(makeTypedTree.Ident(TermRef(sym.owner.thisType, sym.name, sym.signature))))

    def makeChild(sym: Symbol)(implicit ctx: Context) =
      apply(defn.ChildAnnot, List(makeTypedTree.Ident(NamedType(sym.owner.thisType, sym.name))))
  }

  def makeLiteralAnnotArg(const: Constant): TypedTree = ???

  def makeArrayAnnotArg(elems: Array[TypedTree]): TypedTree = ???

  def makeNestedAnnotArg(annot: Annotation): TypedTree = annot.tree

  def ThrowsAnnotation(cls: ClassSymbol)(implicit ctx: Context) =
    Annotation(defn.ThrowsAnnot, makeTypedTree.Ident(TypeRef(cls.owner.thisType, cls.name)))
}