package dotty.tools.dotc.core

import Symbols._, Types._, Positions._, Contexts._, Constants._, TypedTrees.tpd._

object Annotations {

  abstract class Annotation {
    def tree: Tree
    def symbol(implicit ctx: Context): Symbol = tree.tpe.typeSymbol
    def matches(cls: Symbol)(implicit ctx: Context): Boolean = symbol.isNonBottomSubClass(cls)
    def appliesToModule: Boolean = true // for now; see remark in SymDenotations

    def derivedAnnotation(tree: Tree) =
      if (tree eq this.tree) this else Annotation(tree)
  }

  case class ConcreteAnnotation(val tree: Tree) extends Annotation

  case class LazyAnnotation(sym: Symbol)(treeFn: => Tree) extends Annotation {
    private var _tree: Tree = null
    def tree = {
      if (_tree == null) _tree = treeFn
      _tree
    }
    override def symbol(implicit ctx: Context): Symbol = sym
  }

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

    def deferred(sym: Symbol, treeFn: => Tree): Annotation =
      new LazyAnnotation(sym)(treeFn)

    def deferred(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation =
      deferred(atp.typeSymbol, New(atp, args))

    def makeAlias(sym: TermSymbol)(implicit ctx: Context) =
      apply(defn.AliasAnnot, List(Ident(TermRef.withSig(sym.owner.thisType, sym.name, sym.signature).withDenot(sym))))

    def makeChild(sym: Symbol)(implicit ctx: Context) =
      apply(defn.ChildAnnot.typeConstructor.appliedTo(NamedType(sym.owner.thisType, sym.name).withDenot(sym)), Nil)
  }

  def ThrowsAnnotation(cls: ClassSymbol)(implicit ctx: Context) =
    Annotation(defn.ThrowsAnnot, Ident(cls.symbolicRef))
}