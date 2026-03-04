package dotty.tools
package dotc
package core

import Symbols.*, Types.*, Contexts.*, Constants.*, Phases.*
import ast.tpd, tpd.*
import util.Spans.{Span, NoSpan}
import printing.{Showable, Printer}
import printing.Texts.Text
import cc.{isRetainsLike, RetainingAnnotation}
import config.Feature.sourceVersion
import Decorators.*

import scala.annotation.internal.sharable

object Annotations {

  def annotClass(tree: Tree)(using Context) =
    if (tree.symbol.isConstructor) tree.symbol.owner
    else tree.tpe.typeSymbol

  abstract class Annotation extends Showable {
    def tree(using Context): Tree

    def symbol(using Context): Symbol = annotClass(tree)

    def hasSymbol(sym: Symbol)(using Context) = symbol == sym

    def matches(cls: Symbol)(using Context): Boolean = symbol.derivesFrom(cls)

    def appliesToModule: Boolean = true // for now; see remark in SymDenotations

    def derivedAnnotation(tree: Tree)(using Context): Annotation =
      if (tree eq this.tree) this else Annotation(tree)

    /** All term arguments of this annotation in a single flat list */
    def arguments(using Context): List[Tree] = tpd.allTermArguments(tree)

    /** All type arguments of this annotation in a single flat list */
    def argumentTypes(using Context): List[Type] =
      tpd.allArguments(tree).filterConserve(_.isType).tpes

    def argument(i: Int)(using Context): Option[Tree] = {
      val args = arguments
      if (i < args.length) Some(args(i)) else None
    }
    def argumentConstant(i: Int)(using Context): Option[Constant] =
      for case ConstantType(c) <- argument(i).map(stripCast(_).tpe.widenTermRefExpr.normalized) yield c

    def argumentConstantString(i: Int)(using Context): Option[String] =
      for (case Constant(s: String) <- argumentConstant(i)) yield s

    /** The tree evaluation is in progress. */
    def isEvaluating: Boolean = false

    /** The tree evaluation has finished. */
    def isEvaluated: Boolean = true

    /** Normally, applies a type map to all tree nodes of this annotation, but can
     *  be overridden. Returns EmptyAnnotation if type type map produces a range
     *  type, since ranges cannot be types of trees.
     */
    def mapWith(tm: TypeMap)(using Context): Annotation =
      tpd.allArguments(tree) match
        case Nil => this
        case arg :: Nil if symbol.isRetainsLike =>
          assert(false, s"unexpected symbol $symbol for ConcreteAnnotation $this in ${ctx.source}, this should be a CompactAnnotation")
        case args =>
          // Checks if `tm` would result in any change by applying it to types
          // inside the annotations' arguments and checking if the resulting types
          // are different.
          val findDiff = new TreeAccumulator[Type]:
            def apply(x: Type, tree: Tree)(using Context): Type =
              if tm.isRange(x) then x
              else
                val tp1 = tm(tree.tpe)
                foldOver(if !tp1.exists || tp1.eql(tree.tpe) then x else tp1, tree)
          val diff = findDiff(NoType, args)
          if tm.isRange(diff) then EmptyAnnotation
          else if diff.exists then derivedAnnotation(tm.mapOver(tree))
          else this
    end mapWith

    /** Does this annotation refer to a parameter of `tl`? */
    def refersToParamOf(tl: TermLambda)(using Context): Boolean =
      val acc = new TreeAccumulator[Boolean]:
        def apply(x: Boolean, t: Tree)(using Context) =
          if x then true
          else if t.isType then refersToLambdaParam(t.tpe, tl)
          else t match
            case id: (Ident | This) => isLambdaParam(id.tpe.stripped, tl)
            case _ => foldOver(x, t)

      tpd.allArguments(tree).exists(acc(false, _))
    end refersToParamOf

    /** A string representation of the annotation. Overridden in BodyAnnotation.
     */
    def toText(printer: Printer): Text = printer.annotText(this)

    def ensureCompleted(using Context): Unit = tree

    def sameAnnotation(that: Annotation)(using Context): Boolean =
      symbol == that.symbol && tree.sameTree(that.tree)

    def hasOneOfMetaAnnotation(metaSyms: Set[Symbol], orNoneOf: Set[Symbol] = Set.empty)(using Context): Boolean = atPhaseNoLater(erasurePhase) {
      def go(metaSyms: Set[Symbol]) =
        def recTp(tp: Type): Boolean = tp.dealiasKeepAnnots match
          case AnnotatedType(parent, metaAnnot) => metaSyms.exists(metaAnnot.matches) || recTp(parent)
          case _ => false
        def rec(tree: Tree): Boolean = methPart(tree) match
          case New(tpt) => rec(tpt)
          case Select(qual, _) => rec(qual)
          case Annotated(arg, metaAnnot) => metaSyms.exists(metaAnnot.tpe.classSymbol.derivesFrom) || rec(arg)
          case t @ Ident(_) => recTp(t.tpe)
          case Typed(expr, _) => rec(expr)
          case _ => false
        metaSyms.exists(symbol.hasAnnotation) || rec(tree)
      go(metaSyms) || orNoneOf.nonEmpty && !go(orNoneOf)
    }

    /** Operations for hash-consing, can be overridden */
    def hash: Int = System.identityHashCode(this)
    def eql(that: Annotation) = this eq that
  }

  case class ConcreteAnnotation(t: Tree) extends Annotation:
    def tree(using Context): Tree = t

  /** A class for optimized, compact annotations that are defined by a type
   *  instead of a tree. This makes mapping such annotations a lot faster and safer.
   *  In fact, in retrospect, most annotations would better be represented as
   *  CompactAnnotations.
   *
   *  CompactAnnotation is extended by cc.RetainingAnnotation, which is reserved
   *  for @retains, @retainsByName and @retainsCap. For now there are no
   *  CompactAnnotations other than RetainingAnnotations but this could be changed
   *  in the future, after 3.9 has shipped.
   */
  class CompactAnnotation(val tpe: Type) extends Annotation:
    assert(tpe.isInstanceOf[AppliedType | TypeRef], tpe)

    def tree(using Context) = TypeTree(tpe)

    def oldTree(using Context): Tree =
      New(tpe, Nil).withSpan(NoSpan)

    override def symbol(using Context) = tpe.typeSymbol

    override def derivedAnnotation(tree: Tree)(using Context): Annotation =
      derivedAnnotation(tree.tpe)

    def derivedAnnotation(tp: Type)(using Context): Annotation =
      if tp eq this.tpe then this else CompactAnnotation(tp)

    override def arguments(using Context): List[Tree] =
      argumentTypes.map(TypeTree(_))

    override def argumentTypes(using Context): List[Type] = tpe.argInfos

    def argumentType(i: Int)(using Context): Type =
      val args = argumentTypes
      if i < args.length then args(i) else NoType

    override def argumentConstant(i: Int)(using Context): Option[Constant] =
      argumentType(i).normalized match
        case ConstantType(c) => Some(c)
        case _ => None

    /** A hook to transform the type argument of a mapped annotation. Overridden in
     *  RetainingAnnotation to avoid compilation time blowups for annotations that
     *  are not valid capture annotations.
     */
    protected def sanitize(tp: Type)(using Context): Type = tp

    protected def mapWithCtd(tm: TypeMap)(using Context): Annotation = tm(tpe) match
      case tp1 @ AppliedType(tycon, args) =>
        derivedAnnotation(tp1.derivedAppliedType(tycon, args.mapConserve(sanitize)))
      case tp1: TypeRef =>
        derivedAnnotation(tp1)
      case _ =>
        EmptyAnnotation

    override def mapWith(tm: TypeMap)(using Context): Annotation =
      assert(!symbol.isRetainsLike)
      mapWithCtd(tm)

    override def refersToParamOf(tl: TermLambda)(using Context): Boolean =
      refersToLambdaParam(tpe, tl)

    override def hash: Int = tpe.hash
    override def eql(that: Annotation) = that match
      case that: CompactAnnotation => this.tpe `eql` that.tpe
      case _ => false

  object CompactAnnotation:
    def apply(tp: Type)(using Context): CompactAnnotation =
      if tp.typeSymbol.isRetainsLike then RetainingAnnotation(tp)
      else new CompactAnnotation(tp)
    def apply(tree: Tree)(using Context): CompactAnnotation =
      val argTypes = tpd.allArguments(tree).map(_.tpe)
      apply(annotClass(tree).typeRef.appliedTo(argTypes))
  end CompactAnnotation

  private def isLambdaParam(t: Type, tl: TermLambda): Boolean = t match
    case TermParamRef(tl1, _) => tl eq tl1
    case _ => false

  private def refersToLambdaParam(tp: Type, tl: TermLambda)(using Context): Boolean =
    tp.existsPart(isLambdaParam(_, tl), stopAt = StopAt.Static)

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

  class DeferredSymAndTree(symFn: Context ?=> Symbol, treeFn: Context ?=> Tree)
  extends LazyAnnotation:
    protected var mySym: Symbol | (Context ?=> Symbol) | Null = ctx ?=> symFn(using ctx)
    protected var myTree: Tree | (Context ?=> Tree) | Null = ctx ?=> treeFn(using ctx)

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

    def apply(tree: Tree)(using Context): Annotation = tree match
      case tree: TypeTree =>
        CompactAnnotation(tree.tpe)
      case _ =>
        if annotClass(tree).isRetainsLike then CompactAnnotation(tree)
        else ConcreteAnnotation(tree)

    def apply(cls: ClassSymbol, span: Span)(using Context): Annotation =
      apply(cls, Nil, span)

    def apply(cls: ClassSymbol, arg: Tree, span: Span)(using Context): Annotation =
      apply(cls, arg :: Nil, span)

    def apply(cls: ClassSymbol, args: List[Tree], span: Span)(using Context): Annotation =
      apply(cls.typeRef, args, span)

    def apply(atp: Type, arg: Tree, span: Span)(using Context): Annotation =
      apply(atp, arg :: Nil, span)

    def apply(atp: Type, args: List[Tree], span: Span)(using Context): Annotation =
      if atp.typeSymbol.isRetainsLike && args.isEmpty
      then RetainingAnnotation(atp)
      else apply(New(atp, args).withSpan(span))

    /** Create an annotation where the tree is computed lazily. */
    def deferred(sym: Symbol)(treeFn: Context ?=> Tree): Annotation =
      new LazyAnnotation {
        protected var myTree: Tree | (Context ?=> Tree) | Null = ctx ?=> treeFn(using ctx)
        protected var mySym: Symbol | (Context ?=> Symbol) | Null = sym
      }

    /** Create an annotation where the symbol and the tree are computed lazily. */
    def deferredSymAndTree(symFn: Context ?=> Symbol)(treeFn: Context ?=> Tree): Annotation =
      DeferredSymAndTree(symFn, treeFn)

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
  }

  /** An annotation that is used as a result of mapping annotations
   *  to indicate that the resulting typemap should drop the annotation
   *  (in derivedAnnotatedType).
   */
  @sharable val EmptyAnnotation = ConcreteAnnotation(EmptyTree)

  def ThrowsAnnotation(cls: ClassSymbol)(using Context): Annotation = {
    val tref = cls.typeRef
    Annotation(defn.ThrowsAnnot.typeRef.appliedTo(tref), Ident(tref), cls.span)
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

  object ExperimentalAnnotation {

    /** Create an instance of `@experimental(<msg>)` */
    def apply(msg: String, span: Span)(using Context): Annotation =
      Annotation(defn.ExperimentalAnnot, Literal(Constant(msg)), span)

    /** Matches and extracts the message from an instance of `@experimental(msg)`
     *  Returns `Some("")` for `@experimental` with no message.
     */
    def unapply(a: Annotation)(using Context): Option[String] =
      if a.symbol ne defn.ExperimentalAnnot then
        None
      else a.argumentConstant(0) match
        case Some(Constant(msg: String)) => Some(msg)
        case _ => Some("")

    /** Makes a copy of the `@experimental(msg)` annotation on `sym`
     *  None is returned if the symbol does not have an `@experimental` annotation.
     */
    def copy(sym: Symbol)(using Context): Option[Annotation] =
      sym.getAnnotation(defn.ExperimentalAnnot).map {
        case annot @ ExperimentalAnnotation(msg) => ExperimentalAnnotation(msg, annot.tree.span)
      }
  }

  object PreviewAnnotation {
    /** Matches and extracts the message from an instance of `@preview(msg)`
     *  Returns `Some("")` for `@preview` with no message.
     */
    def unapply(a: Annotation)(using Context): Option[String] =
      if a.symbol ne defn.PreviewAnnot then
        None
      else a.argumentConstant(0) match
        case Some(Constant(msg: String)) => Some(msg)
        case _ => Some("")
  }
}
