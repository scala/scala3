package dotty.tools.dotc
package core

import Symbols._, Types._, util.Positions._, Contexts._, Constants._, ast.tpd._

object Annotations {

  abstract class Annotation {
    def tree(implicit ctx: Context): Tree
    def symbol(implicit ctx: Context): Symbol = tree.tpe.typeSymbol
    def matches(cls: Symbol)(implicit ctx: Context): Boolean = symbol.derivesFrom(cls)
    def appliesToModule: Boolean = true // for now; see remark in SymDenotations

    def derivedAnnotation(tree: Tree)(implicit ctx: Context) =
      if (tree eq this.tree) this else Annotation(tree)
  }

  case class ConcreteAnnotation(t: Tree) extends Annotation {
    def tree(implicit ctx: Context): Tree = t
  }

  case class LazyAnnotation(sym: Symbol)(treeFn: Context => Tree) extends Annotation {
    private var myTree: Tree = null
    def tree(implicit ctx: Context) = {
      if (myTree == null) myTree = treeFn(ctx)
      myTree
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
      apply(cls.typeRef, args)

    def apply(atp: Type, arg: Tree)(implicit ctx: Context): Annotation =
      apply(atp, arg :: Nil)

    def apply(atp: Type, arg1: Tree, arg2: Tree)(implicit ctx: Context): Annotation =
      apply(atp, arg1 :: arg2 :: Nil)

    def apply(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation =
      apply(New(atp, args))

    def deferred(sym: Symbol, treeFn: Context => Tree)(implicit ctx: Context): Annotation =
      new LazyAnnotation(sym)(treeFn)

    def deferred(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation =
      deferred(atp.classSymbol, implicit ctx => New(atp, args))

    def makeAlias(sym: TermSymbol)(implicit ctx: Context) =
      apply(defn.AliasAnnot, List(Ident(TermRef.withSig(sym.owner.thisType, sym.name, sym.signature, sym))))

    def makeChild(sym: Symbol)(implicit ctx: Context) =
      apply(defn.ChildAnnot.typeRef.appliedTo(sym.owner.thisType.select(sym.name, sym)), Nil)
  }

  def ThrowsAnnotation(cls: ClassSymbol)(implicit ctx: Context) = {
    val tref = cls.typeRef
    Annotation(defn.ThrowsAnnot.typeRef.appliedTo(tref), Ident(tref))
  }
}