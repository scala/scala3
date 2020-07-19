package dottyBench.tools.dotc
package core

import Symbols._, Types._, Contexts._, Constants._, ast.tpd._, Phases._
import config.ScalaVersion
import StdNames._
import dottyBench.tools.dotc.ast.tpd
import scala.util.Try
import util.Spans.Span

object Annotations {

  def annotClass(tree: Tree)(using Ctx, CState) =
    if (tree.symbol.isConstructor) tree.symbol.owner
    else tree.tpe.typeSymbol

  abstract class Annotation {
    def tree(using Ctx, CState): Tree

    def symbol(using Ctx, CState): Symbol = annotClass(tree)

    def matches(cls: Symbol)(using Ctx, CState): Boolean = symbol.derivesFrom(cls)

    def appliesToModule: Boolean = true // for now; see remark in SymDenotations

    def derivedAnnotation(tree: Tree)(using Ctx, CState): Annotation =
      if (tree eq this.tree) this else Annotation(tree)

    def arguments(using Ctx, CState): List[Tree] = ast.tpd.arguments(tree)

    def argument(i: Int)(using Ctx, CState): Option[Tree] = {
      val args = arguments
      if (i < args.length) Some(args(i)) else None
    }
    def argumentConstant(i: Int)(using Ctx, CState): Option[Constant] =
      for (ConstantType(c) <- argument(i) map (_.tpe)) yield c

    /** The tree evaluaton is in progress. */
    def isEvaluating: Boolean = false

    /** The tree evaluation has finished. */
    def isEvaluated: Boolean = true

    def ensureCompleted(using Ctx, CState): Unit = tree

    def sameAnnotation(that: Annotation)(using Ctx, CState): Boolean =
      symbol == that.symbol && tree.sameTree(that.tree)
  }

  case class ConcreteAnnotation(t: Tree) extends Annotation {
    def tree(using Ctx, CState): Tree = t
  }

  abstract class LazyAnnotation extends Annotation {
    protected var mySym: Symbol | ((Ctx, CState) ?=> Symbol)
    override def symbol(using parentCtx: Ctx, cs: CState): Symbol =
      assert(mySym != null)
      mySym match {
        case symFn: ((Ctx, CState) ?=> Symbol) @unchecked =>
          mySym = null
          mySym = atPhaseNoLater(picklerPhase)(symFn)
        case sym: Symbol if sym.defRunId != currentRunId(using parentCtx) =>
          mySym = sym.denot.current.symbol
        case _ =>
      }
      mySym.asInstanceOf[Symbol]

    protected var myTree: Tree | ((Ctx, CState) ?=> Tree)
    def tree(using Ctx, CState): Tree =
      assert(myTree != null)
      myTree match {
        case treeFn: ((Ctx, CState) ?=> Tree) @unchecked =>
          myTree = null
          myTree = atPhaseNoLater(picklerPhase)(treeFn)
        case _ =>
      }
      myTree.asInstanceOf[Tree]

    override def isEvaluating: Boolean = myTree == null
    override def isEvaluated: Boolean = myTree.isInstanceOf[Tree @unchecked]
  }

  /** An annotation indicating the body of a right-hand side,
   *  typically of an inline method. Treated specially in
   *  pickling/unpickling and TypeTreeMaps
   */
  abstract class BodyAnnotation extends Annotation {
    override def symbol(using Ctx, CState): ClassSymbol = defn.BodyAnnot
    override def derivedAnnotation(tree: Tree)(using Ctx, CState): Annotation =
      if (tree eq this.tree) this else ConcreteBodyAnnotation(tree)
    override def arguments(using Ctx, CState): List[Tree] = Nil
    override def ensureCompleted(using Ctx, CState): Unit = ()
  }

  class ConcreteBodyAnnotation(body: Tree) extends BodyAnnotation {
    def tree(using Ctx, CState): Tree = body
  }

  abstract class LazyBodyAnnotation extends BodyAnnotation {
    // Copy-pasted from LazyAnnotation to avoid having to turn it into a trait
    protected var myTree: Tree | ((Ctx, CState) ?=> Tree)
    def tree(using Ctx, CState): Tree =
      assert(myTree != null)
      myTree match {
        case treeFn: ((Ctx, CState) ?=> Tree) @unchecked =>
          myTree = null
          myTree = atPhaseNoLater(picklerPhase)(treeFn)
        case _ =>
      }
      myTree.asInstanceOf[Tree]

    override def isEvaluating: Boolean = myTree == null
    override def isEvaluated: Boolean = myTree.isInstanceOf[Tree @unchecked]
  }

  object LazyBodyAnnotation {
    def apply(bodyFn: (Ctx, CState) ?=> Tree): LazyBodyAnnotation =
      new LazyBodyAnnotation:
        protected var myTree: Tree | ((Ctx, CState) ?=> Tree) = (using ctx: Ctx, cs: CState) => bodyFn
  }

  object Annotation {

    def apply(tree: Tree): ConcreteAnnotation = ConcreteAnnotation(tree)

    def apply(cls: ClassSymbol)(using Ctx, CState): Annotation =
      apply(cls, Nil)

    def apply(cls: ClassSymbol, arg: Tree)(using Ctx, CState): Annotation =
      apply(cls, arg :: Nil)

    def apply(cls: ClassSymbol, arg1: Tree, arg2: Tree)(using Ctx, CState): Annotation =
      apply(cls, arg1 :: arg2 :: Nil)

    def apply(cls: ClassSymbol, args: List[Tree])(using Ctx, CState): Annotation =
      apply(cls.typeRef, args)

    def apply(atp: Type, arg: Tree)(using Ctx, CState): Annotation =
      apply(atp, arg :: Nil)

    def apply(atp: Type, arg1: Tree, arg2: Tree)(using Ctx, CState): Annotation =
      apply(atp, arg1 :: arg2 :: Nil)

    def apply(atp: Type, args: List[Tree])(using Ctx, CState): Annotation =
      apply(New(atp, args))

    /** Create an annotation where the tree is computed lazily. */
    def deferred(sym: Symbol)(treeFn: (Ctx, CState) ?=> Tree)(using Ctx, CState): Annotation =
      new LazyAnnotation {
        protected var myTree: Tree | ((Ctx, CState) ?=> Tree) = (using ctx: Ctx, cs: CState) => treeFn
        protected var mySym: Symbol | ((Ctx, CState) ?=> Symbol) = sym
      }

    /** Create an annotation where the symbol and the tree are computed lazily. */
    def deferredSymAndTree(symFn: (Ctx, CState) ?=> Symbol)(treeFn: (Ctx, CState) ?=> Tree)(using Ctx, CState): Annotation =
      new LazyAnnotation {
        protected var mySym: Symbol | ((Ctx, CState) ?=> Symbol) = (using ctx: Ctx, cs: CState) => symFn
        protected var myTree: Tree | ((Ctx, CState) ?=> Tree) = (using ctx: Ctx, cs: CState) => treeFn // Dotty deviation: cannot infer 2nd parameter type, infers same as first
      }

    def deferred(atp: Type, args: List[Tree])(using Ctx, CState): Annotation =
      deferred(atp.classSymbol)(New(atp, args))

    def deferredResolve(atp: Type, args: List[ast.untpd.Tree])(using Ctx, CState): Annotation =
      deferred(atp.classSymbol)(ast.untpd.resolveConstructor(atp, args))

    /** Extractor for child annotations */
    object Child {

      /** A deferred annotation to the result of a given child computation */
      def later(delayedSym: (Ctx, CState) ?=> Symbol, span: Span)(using Ctx, CState): Annotation = {
        def makeChildLater(using Ctx, CState) = {
          val sym = delayedSym
          New(defn.ChildAnnot.typeRef.appliedTo(sym.owner.thisType.select(sym.name, sym)), Nil)
            .withSpan(span)
        }
        deferred(defn.ChildAnnot)(makeChildLater)
      }

      /** A regular, non-deferred Child annotation */
      def apply(sym: Symbol, span: Span)(using Ctx, CState): Annotation = later(sym, span)

      def unapply(ann: Annotation)(using Ctx, CState): Option[Symbol] =
        if (ann.symbol == defn.ChildAnnot) {
          val AppliedType(_, (arg: NamedType) :: Nil) = ann.tree.tpe
          Some(arg.symbol)
        }
        else None
    }

    def makeSourceFile(path: String)(using Ctx, CState): Annotation =
      apply(defn.SourceFileAnnot, Literal(Constant(path)))
  }

  def ThrowsAnnotation(cls: ClassSymbol)(using Ctx, CState): Annotation = {
    val tref = cls.typeRef
    Annotation(defn.ThrowsAnnot.typeRef.appliedTo(tref), Ident(tref))
  }

  /** Extracts the type of the thrown exception from an annotation.
   *
   *  Supports both "old-style" `@throws(classOf[Exception])`
   *  as well as "new-style" `@throws[Exception]("cause")` annotations.
   */
  object ThrownException {
    def unapply(a: Annotation)(using Ctx, CState): Option[Type] =
      if (a.symbol ne defn.ThrowsAnnot)
        None
      else a.argumentConstant(0) match {
        // old-style: @throws(classOf[Exception]) (which is throws[T](classOf[Exception]))
        case Some(Constant(tpe: Type)) =>
          Some(tpe)
        // new-style: @throws[Exception], @throws[Exception]("cause")
        case _ =>
          stripApply(a.tree) match {
            case TypeApply(_, List(tpt)) =>
              Some(tpt.tpe)
            case _ =>
              None
          }
      }
  }
}
