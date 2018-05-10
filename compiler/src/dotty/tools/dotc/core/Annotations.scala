package dotty.tools.dotc
package core

import Symbols._, Types._, util.Positions._, Contexts._, Constants._, ast.tpd._
import config.ScalaVersion
import StdNames._
import dotty.tools.dotc.ast.{tpd, untpd}

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

    def isEvaluated: Boolean = true

    def ensureCompleted(implicit ctx: Context): Unit = tree
  }

  case class ConcreteAnnotation(t: Tree) extends Annotation {
    def tree(implicit ctx: Context): Tree = t
  }

  abstract class LazyAnnotation extends Annotation {
    override def symbol(implicit ctx: Context): Symbol
    def complete(implicit ctx: Context): Tree

    private[this] var myTree: Tree = null
    def tree(implicit ctx: Context) = {
      if (myTree == null) myTree = complete(ctx)
      myTree
    }

    override def isEvaluated = myTree != null
  }

  /** An annotation indicating the body of a right-hand side,
   *  typically of an inline method. Treated specially in
   *  pickling/unpickling and TypeTreeMaps
   */
  abstract class BodyAnnotation extends Annotation {
    override def symbol(implicit ctx: Context) = defn.BodyAnnot
    override def derivedAnnotation(tree: Tree)(implicit ctx: Context) =
      if (tree eq this.tree) this else ConcreteBodyAnnotation(tree)
    override def arguments(implicit ctx: Context) = Nil
    override def ensureCompleted(implicit ctx: Context) = ()
  }

  case class ConcreteBodyAnnotation(body: Tree) extends BodyAnnotation {
    def tree(implicit ctx: Context) = body
  }

  case class LazyBodyAnnotation(private var bodyExpr: Context => Tree) extends BodyAnnotation {
    private[this] var evaluated = false
    private[this] var myBody: Tree = _
    def tree(implicit ctx: Context) = {
      if (evaluated) assert(myBody != null)
      else {
        evaluated = true
        myBody = bodyExpr(ctx)
        bodyExpr = null
      }
      myBody
    }
    override def isEvaluated = evaluated
  }

  object Annotation {

    def apply(tree: Tree) = ConcreteAnnotation(tree)

    def apply(cls: ClassSymbol)(implicit ctx: Context): Annotation =
      apply(cls, Nil)

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
      tpd.applyOverloaded(New(atp.typeConstructor), nme.CONSTRUCTOR, args, targs, atp, isAnnotConstructor = true)
    }

    def applyResolve(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation = {
      apply(resolveConstructor(atp, args))
    }

    /** Create an annotation where the tree is computed lazily. */
    def deferred(sym: Symbol, treeFn: Context => Tree)(implicit ctx: Context): Annotation =
      new LazyAnnotation {
        override def symbol(implicit ctx: Context): Symbol = sym
        def complete(implicit ctx: Context) = treeFn(ctx)
      }

    /** Create an annotation where the symbol and the tree are computed lazily. */
    def deferredSymAndTree(symf: Context => Symbol, treeFn: Context => Tree)(implicit ctx: Context): Annotation =
      new LazyAnnotation {
        private[this] var mySym: Symbol = _

        override def symbol(implicit ctx: Context): Symbol = {
          if (mySym == null || mySym.defRunId != ctx.runId) {
            mySym = symf(ctx)
            assert(mySym != null)
          }
          mySym
        }
        def complete(implicit ctx: Context) = treeFn(ctx)
      }

    def deferred(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation =
      deferred(atp.classSymbol, implicit ctx => New(atp, args))

    def deferredResolve(atp: Type, args: List[Tree])(implicit ctx: Context): Annotation =
      deferred(atp.classSymbol, implicit ctx => resolveConstructor(atp, args))

    def makeAlias(sym: TermSymbol)(implicit ctx: Context) =
      apply(defn.AliasAnnot, List(
        ref(TermRef(sym.owner.thisType, sym.name, sym.denot))))

    /** Extractor for child annotations */
    object Child {

      /** A deferred annotation to the result of a given child computation */
      def apply(delayedSym: Context => Symbol)(implicit ctx: Context): Annotation = {
        def makeChildLater(implicit ctx: Context) = {
          val sym = delayedSym(ctx)
          New(defn.ChildAnnotType.appliedTo(sym.owner.thisType.select(sym.name, sym.denot)), Nil)
        }
        deferred(defn.ChildAnnot, implicit ctx => makeChildLater(ctx))
      }

      /** A regular, non-deferred Child annotation */
      def apply(sym: Symbol)(implicit ctx: Context): Annotation = apply(_ => sym)

      def unapply(ann: Annotation)(implicit ctx: Context): Option[Symbol] =
        if (ann.symbol == defn.ChildAnnot) {
          val AppliedType(tycon, (arg: NamedType) :: Nil) = ann.tree.tpe
          Some(arg.symbol)
        }
        else None
    }

    def makeSourceFile(path: String)(implicit ctx: Context) =
      apply(defn.SourceFileAnnot, Literal(Constant(path)))
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
