package dotty.tools.dotc
package core

import Symbols._, Types._, util.Positions._, Contexts._, Constants._, ast.tpd._
import config.ScalaVersion
import StdNames._
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.typer.ProtoTypes.FunProtoTyped

object Annotations {

  abstract class Annotation {
    def tree(implicit ctx: Context): Tree
    def symbol(implicit ctx: Context): Symbol =
      if (tree.symbol.isConstructor) tree.symbol.owner
      else tree.tpe.typeSymbol
    def matches(cls: Symbol)(implicit ctx: Context): Boolean = symbol.derivesFrom(cls)
    def appliesToModule: Boolean = true // for now; see remark in SymDenotations

    def derivedAnnotation(tree: Tree)(implicit ctx: Context) =
      if (tree eq this.tree) this else Annotation(tree)

    def arguments(implicit ctx: Context) = ast.tpd.arguments(tree)
    def argument(i: Int)(implicit ctx: Context): Option[Tree] = {
      val args = arguments
      if (i < args.length) Some(args(i)) else None
    }
    def argumentConstant(i: Int)(implicit ctx: Context): Option[Constant] =
      for (ConstantType(c) <- argument(i) map (_.tpe)) yield c
  }

  case class ConcreteAnnotation(t: Tree) extends Annotation {
    def tree(implicit ctx: Context): Tree = t
  }

  abstract case class LazyAnnotation(sym: Symbol) extends Annotation {
    private var myTree: Tree = null
    def tree(implicit ctx: Context) = {
      if (myTree == null) myTree = complete(ctx)
      myTree
    }
    def complete(implicit ctx: Context): Tree
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

    private def resolveConstructor(atp: Type, args:List[Tree])(implicit ctx: Context): Tree = {
      val targs = atp.argTypes
      tpd.applyOverloaded(New(atp withoutArgs targs), nme.CONSTRUCTOR, args, targs, atp, isAnnotConstructor = true)
    }

    def applyResolve(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation = {
      apply(resolveConstructor(atp, args))
    }

    def deferred(sym: Symbol, treeFn: Context => Tree)(implicit ctx: Context): Annotation =
      new LazyAnnotation(sym) {
        def complete(implicit ctx: Context) = treeFn(ctx)
      }

    def deferred(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation =
      deferred(atp.classSymbol, implicit ctx => New(atp, args))

    def deferredResolve(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation =
      deferred(atp.classSymbol, implicit ctx => resolveConstructor(atp, args))

    def makeAlias(sym: TermSymbol)(implicit ctx: Context) =
      apply(defn.AliasAnnot, List(
        ref(TermRef.withSigAndDenot(sym.owner.thisType, sym.name, sym.signature, sym))))

    def makeChild(sym: Symbol)(implicit ctx: Context) =
      deferred(defn.ChildAnnot,
        implicit ctx => New(defn.ChildAnnotType.appliedTo(sym.owner.thisType.select(sym.name, sym)), Nil))
  }

  def ThrowsAnnotation(cls: ClassSymbol)(implicit ctx: Context) = {
    val tref = cls.typeRef
    Annotation(defn.ThrowsAnnotType.appliedTo(tref), Ident(tref))
  }

  /** A decorator that provides queries for specific annotations
   *  of a symbol.
   */
  implicit class AnnotInfo(val sym: Symbol) extends AnyVal {

    def isDeprecated(implicit ctx: Context) =
      sym.hasAnnotation(defn.DeprecatedAnnot)

    def deprecationMessage(implicit ctx: Context) =
      for (annot <- sym.getAnnotation(defn.DeprecatedAnnot);
           arg <- annot.argumentConstant(0))
      yield arg.stringValue

    def migrationVersion(implicit ctx: Context) =
      for (annot <- sym.getAnnotation(defn.MigrationAnnot);
           arg <- annot.argumentConstant(1))
      yield ScalaVersion.parse(arg.stringValue)

    def migrationMessage(implicit ctx: Context) =
      for (annot <- sym.getAnnotation(defn.MigrationAnnot);
           arg <- annot.argumentConstant(0))
      yield ScalaVersion.parse(arg.stringValue)
  }
}
