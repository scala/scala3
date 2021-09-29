package dotty.tools.dotc
package core

import Symbols._, Types._, Contexts._, Constants._
import dotty.tools.dotc.ast.tpd, tpd.*
import util.Spans.Span
import printing.{Showable, Printer}
import printing.Texts.Text
import annotation.internal.sharable

object Annotations {

  def annotClass(tree: Tree)(using Context) =
    if (tree.symbol.isConstructor) tree.symbol.owner
    else tree.tpe.typeSymbol

  abstract class Annotation extends Showable {
    def tree(using Context): Tree

    def symbol(using Context): Symbol = annotClass(tree)

    def matches(cls: Symbol)(using Context): Boolean = symbol.derivesFrom(cls)

    def appliesToModule: Boolean = true // for now; see remark in SymDenotations

    def derivedAnnotation(tree: Tree)(using Context): Annotation =
      if (tree eq this.tree) this else Annotation(tree)

    /** All arguments to this annotation in a single flat list */
    def arguments(using Context): List[Tree] = tpd.allArguments(tree)

    def argument(i: Int)(using Context): Option[Tree] = {
      val args = arguments
      if (i < args.length) Some(args(i)) else None
    }
    def argumentConstant(i: Int)(using Context): Option[Constant] =
      for (case ConstantType(c) <- argument(i) map (_.tpe.widenTermRefExpr.normalized)) yield c

    def argumentConstantString(i: Int)(using Context): Option[String] =
      for (case Constant(s: String) <- argumentConstant(i)) yield s

    /** The tree evaluaton is in progress. */
    def isEvaluating: Boolean = false

    /** The tree evaluation has finished. */
    def isEvaluated: Boolean = true

    /** Normally, applies a type map to all tree nodes of this annotation, but can
     *  be overridden. Returns EmptyAnnotation if type type map produces a range
     *  type, since ranges cannot be types of trees.
     */
    def mapWith(tm: TypeMap)(using Context) =
      val args = arguments
      if args.isEmpty then this
      else
        val findDiff = new TreeAccumulator[Type]:
          def apply(x: Type, tree: Tree)(using Context): Type =
            if tm.isRange(x) then x
            else
              val tp1 = tm(tree.tpe)
              foldOver(if tp1 =:= tree.tpe then x else tp1, tree)
        val diff = findDiff(NoType, args)
        if tm.isRange(diff) then EmptyAnnotation
        else if diff.exists then derivedAnnotation(tm.mapOver(tree))
        else this

    /** Does this annotation refer to a parameter of `tl`? */
    def refersToParamOf(tl: TermLambda)(using Context): Boolean =
      val args = arguments
      if args.isEmpty then false
      else tree.existsSubTree {
        case id: Ident => id.tpe match
          case TermParamRef(tl1, _) => tl eq tl1
          case _ => false
        case _ => false
      }

    /** A string representation of the annotation. Overridden in BodyAnnotation.
     */
    def toText(printer: Printer): Text = printer.annotText(this)

    def ensureCompleted(using Context): Unit = tree

    def sameAnnotation(that: Annotation)(using Context): Boolean =
      symbol == that.symbol && tree.sameTree(that.tree)

    /** Operations for hash-consing, can be overridden */
    def hash: Int = System.identityHashCode(this)
    def eql(that: Annotation) = this eq that
  }

  case class ConcreteAnnotation(t: Tree) extends Annotation:
    def tree(using Context): Tree = t

  abstract class LazyAnnotation extends Annotation {
    protected var mySym: Symbol | (Context ?=> Symbol) | Null
    override def symbol(using parentCtx: Context): Symbol =
      assert(mySym != null)
      mySym match {
        case symFn: (Context ?=> Symbol) @unchecked =>
          mySym = null
          mySym = atPhaseBeforeTransforms(symFn)
            // We should always produce the same annotation tree, no matter when the
            // annotation is evaluated. Setting the phase to a pre-transformation phase
            // seems to be enough to ensure this (note that after erasure, `ctx.typer`
            // will be the Erasure typer, but that doesn't seem to affect the annotation
            // trees we create, so we leave it as is)
        case sym: Symbol if sym.defRunId != parentCtx.runId =>
          mySym = sym.denot.current.symbol
        case _ =>
      }
      mySym.asInstanceOf[Symbol]

    protected var myTree: Tree | (Context ?=> Tree) | Null
    def tree(using Context): Tree =
      assert(myTree != null)
      myTree match {
        case treeFn: (Context ?=> Tree) @unchecked =>
          myTree = null
          myTree = atPhaseBeforeTransforms(treeFn)
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
    override def symbol(using Context): ClassSymbol = defn.BodyAnnot
    override def derivedAnnotation(tree: Tree)(using Context): Annotation =
      if (tree eq this.tree) this else ConcreteBodyAnnotation(tree)
    override def arguments(using Context): List[Tree] = Nil
    override def ensureCompleted(using Context): Unit = ()
    override def toText(printer: Printer): Text = "@Body"
  }

  class ConcreteBodyAnnotation(body: Tree) extends BodyAnnotation {
    def tree(using Context): Tree = body
  }

  abstract class LazyBodyAnnotation extends BodyAnnotation {
    // Copy-pasted from LazyAnnotation to avoid having to turn it into a trait
    protected var myTree: Tree | (Context ?=> Tree) | Null
    def tree(using Context): Tree =
      assert(myTree != null)
      myTree match {
        case treeFn: (Context ?=> Tree) @unchecked =>
          myTree = null
          myTree = atPhaseBeforeTransforms(treeFn)
        case _ =>
      }
      myTree.asInstanceOf[Tree]

    override def isEvaluating: Boolean = myTree == null
    override def isEvaluated: Boolean = myTree.isInstanceOf[Tree @unchecked]
  }

  object LazyBodyAnnotation {
    def apply(bodyFn: Context ?=> Tree): LazyBodyAnnotation =
      new LazyBodyAnnotation:
        protected var myTree: Tree | (Context ?=> Tree) | Null = ctx ?=> bodyFn(using ctx)
  }

  object Annotation {

    def apply(tree: Tree): ConcreteAnnotation = ConcreteAnnotation(tree)

    def apply(cls: ClassSymbol)(using Context): Annotation =
      apply(cls, Nil)

    def apply(cls: ClassSymbol, arg: Tree)(using Context): Annotation =
      apply(cls, arg :: Nil)

    def apply(cls: ClassSymbol, arg1: Tree, arg2: Tree)(using Context): Annotation =
      apply(cls, arg1 :: arg2 :: Nil)

    def apply(cls: ClassSymbol, args: List[Tree])(using Context): Annotation =
      apply(cls.typeRef, args)

    def apply(atp: Type, arg: Tree)(using Context): Annotation =
      apply(atp, arg :: Nil)

    def apply(atp: Type, arg1: Tree, arg2: Tree)(using Context): Annotation =
      apply(atp, arg1 :: arg2 :: Nil)

    def apply(atp: Type, args: List[Tree])(using Context): Annotation =
      apply(New(atp, args))

    /** Create an annotation where the tree is computed lazily. */
    def deferred(sym: Symbol)(treeFn: Context ?=> Tree)(using Context): Annotation =
      new LazyAnnotation {
        protected var myTree: Tree | (Context ?=> Tree) | Null = ctx ?=> treeFn(using ctx)
        protected var mySym: Symbol | (Context ?=> Symbol) | Null = sym
      }

    /** Create an annotation where the symbol and the tree are computed lazily. */
    def deferredSymAndTree(symFn: Context ?=> Symbol)(treeFn: Context ?=> Tree)(using Context): Annotation =
      new LazyAnnotation {
        protected var mySym: Symbol | (Context ?=> Symbol) | Null = ctx ?=> symFn(using ctx)
        protected var myTree: Tree | (Context ?=> Tree) | Null = ctx ?=> treeFn(using ctx)
      }

    /** Extractor for child annotations */
    object Child {

      /** A deferred annotation to the result of a given child computation */
      def later(delayedSym: Context ?=> Symbol, span: Span)(using Context): Annotation = {
        def makeChildLater(using Context) = {
          val sym = delayedSym
          New(defn.ChildAnnot.typeRef.appliedTo(sym.owner.thisType.select(sym.name, sym)), Nil)
            .withSpan(span)
        }
        deferred(defn.ChildAnnot)(makeChildLater)
      }

      /** A regular, non-deferred Child annotation */
      def apply(sym: Symbol, span: Span)(using Context): Annotation = later(sym, span)

      def unapply(ann: Annotation)(using Context): Option[Symbol] =
        if (ann.symbol == defn.ChildAnnot) {
          val AppliedType(_, (arg: NamedType) :: Nil) = ann.tree.tpe: @unchecked
          Some(arg.symbol)
        }
        else None
    }

    def makeSourceFile(path: String)(using Context): Annotation =
      apply(defn.SourceFileAnnot, Literal(Constant(path)))
  }

  @sharable val EmptyAnnotation = Annotation(EmptyTree)

  def ThrowsAnnotation(cls: ClassSymbol)(using Context): Annotation = {
    val tref = cls.typeRef
    Annotation(defn.ThrowsAnnot.typeRef.appliedTo(tref), Ident(tref))
  }

  /** Extracts the type of the thrown exception from an annotation.
   *
   *  Supports both "old-style" `@throws(classOf[Exception])`
   *  as well as "new-style" `@throws[Exception]("cause")` annotations.
   */
  object ThrownException {
    def unapply(a: Annotation)(using Context): Option[Type] =
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
