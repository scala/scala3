package dotty.tools.dotc
package staging

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.staging.QuoteContext.*
import dotty.tools.dotc.staging.StagingLevel.*
import dotty.tools.dotc.staging.QuoteTypeTags.*
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.util.SrcPos

/** Checks that staging level consistency holds and heals staged types .
 *
 *  Local term references are level consistent if and only if they are used at the same level as their definition.
 *
 *  Local type references can be used at the level of their definition or lower. If used used at a higher level,
 *  it will be healed if possible, otherwise it is inconsistent.
 *
 *  Type healing consists in transforming a level inconsistent type `T` into `summon[Type[T]].Underlying`.
 *
 *  As references to types do not necessarily have an associated tree it is not always possible to replace the types directly.
 *  Instead we always generate a type alias for it and place it at the start of the surrounding quote. This also avoids duplication.
 *  For example:
 *    '{
 *      val x: List[T] = List[T]()
 *      ()
 *    }
 *
 *  is transformed to
 *
 *    '{
 *      type t$1 = summon[Type[T]].Underlying
 *      val x: List[t$1] = List[t$1]();
 *      ()
 *     }
 *
 */
class CrossStageSafety extends TreeMapWithStages {
  import tpd._

  private val InAnnotation = Property.Key[Unit]()

  override def transform(tree: Tree)(using Context): Tree =
    if (tree.source != ctx.source && tree.source.exists)
      transform(tree)(using ctx.withSource(tree.source))
    else if !isInQuoteOrSplice then
      checkAnnotations(tree)
      super.transform(tree)
    else tree match {
      case _: TypeTree =>
        val tp1 = transformTypeAnnotationSplices(tree.tpe)
        val healedType = healType(tree.srcPos)(tp1)
        if healedType == tree.tpe then tree
        else TypeTree(healedType).withSpan(tree.span)
      case _: RefTree | _: SingletonTypeTree if tree.isType =>
        val healedType = healType(tree.srcPos)(tree.tpe)
        if healedType == tree.tpe then tree
        else TypeTree(healedType).withSpan(tree.span)
      case tree: Ident if isWildcardArg(tree) =>
        tree.withType(healType(tree.srcPos)(tree.tpe))
      case tree: Ident => // this is a term Ident
        checkLevelConsistency(tree)
        tree
      case tree: This =>
        checkLevelConsistency(tree)
        tree
      case _: AppliedTypeTree =>
        super.transform(tree) match
          case tree1: AppliedTypeTree if tree1 ne tree =>
            // propagate healed types
            tree1.withType(tree1.tpt.tpe.appliedTo(tree1.args.map(_.tpe)))
          case tree1 => tree1
      case tree: DefDef if tree.symbol.is(Inline) && level > 0 =>
        EmptyTree // Remove inline defs in quoted code. Already fully inlined.
      case tree: ValOrDefDef =>
        checkAnnotations(tree)
        healInfo(tree, tree.tpt.srcPos)
        super.transform(tree)
      case tree: Bind =>
        checkAnnotations(tree)
        healInfo(tree, tree.srcPos)
        super.transform(tree)
      case tree: UnApply =>
        super.transform(tree).withType(healType(tree.srcPos)(tree.tpe))
      case tree: TypeDef if tree.symbol.is(Case) && level > 0 =>
        report.error(reporting.CaseClassInInlinedCode(tree), tree)
        super.transform(tree)
      case _ =>
        super.transform(tree)
    }

  /** Transform quoted trees while maintaining level correctness */
  override protected def transformQuotation(body: Tree, quote: Apply)(using Context): Tree = {
    val taggedTypes = new QuoteTypeTags(quote.span)

    if (ctx.property(InAnnotation).isDefined)
      report.error("Cannot have a quote in an annotation", quote.srcPos)

    val stripAnnotsDeep: TypeMap = new TypeMap:
      def apply(tp: Type): Type = mapOver(tp.stripAnnots)

    def transformBody() =
      val contextWithQuote =
        if level == 0 then contextWithQuoteTypeTags(taggedTypes)(using quoteContext)
        else quoteContext
      val transformedBody = transform(body)(using contextWithQuote)
      taggedTypes.getTypeTags match
        case Nil  => transformedBody
        case tags => tpd.Block(tags, transformedBody).withSpan(body.span)

    if body.isTerm then
      val transformedBody = transformBody()
      // `quoted.runtime.Expr.quote[T](<body>)`  --> `quoted.runtime.Expr.quote[T2](<body2>)`
      val TypeApply(fun, targs) = quote.fun: @unchecked
      val targs2 = targs.map(targ => TypeTree(healType(quote.fun.srcPos)(stripAnnotsDeep(targ.tpe))))
      cpy.Apply(quote)(cpy.TypeApply(quote.fun)(fun, targs2), transformedBody :: Nil)
    else
      body.tpe match
        case DirectTypeOf(termRef) =>
          // Optimization: `quoted.Type.of[x.Underlying](quotes)`  -->  `x`
          ref(termRef).withSpan(quote.span)
        case _ =>
          transformBody() match
            case DirectTypeOf.Healed(termRef) =>
              // Optimization: `quoted.Type.of[@SplicedType type T = x.Underlying; T](quotes)`  -->  `x`
              ref(termRef).withSpan(quote.span)
            case transformedBody =>
              val quotes = quote.args.mapConserve(transform)
              // `quoted.Type.of[<body>](quotes)`  --> `quoted.Type.of[<body2>](quotes)`
              val TypeApply(fun, _) = quote.fun: @unchecked
              cpy.Apply(quote)(cpy.TypeApply(quote.fun)(fun, transformedBody :: Nil), quotes)

  }

  /** Transform splice
   *  - If inside a quote, transform the contents of the splice.
   *  - If inside inlined code, expand the macro code.
   *  - If inside of a macro definition, check the validity of the macro.
   */
  protected def transformSplice(body: Tree, splice: Apply)(using Context): Tree = {
    val body1 = transform(body)(using spliceContext)
    splice.fun match {
      case fun @ TypeApply(_, _ :: Nil) =>
        // Type of the splice itself must also be healed
        // `quoted.runtime.Expr.quote[F[T]](... T ...)`  -->  `internal.Quoted.expr[F[$t]](... T ...)`
        val tp = healType(splice.srcPos)(splice.tpe.widenTermRefExpr)
        cpy.Apply(splice)(cpy.TypeApply(fun)(fun.fun, tpd.TypeTree(tp) :: Nil), body1 :: Nil)
      case f @ Apply(fun @ TypeApply(_, _), quotes :: Nil) =>
        // Type of the splice itself must also be healed
        // `quoted.runtime.Expr.quote[F[T]](... T ...)`  -->  `internal.Quoted.expr[F[$t]](... T ...)`
        val tp = healType(splice.srcPos)(splice.tpe.widenTermRefExpr)
        cpy.Apply(splice)(cpy.Apply(f)(cpy.TypeApply(fun)(fun.fun, tpd.TypeTree(tp) :: Nil), quotes :: Nil), body1 :: Nil)
    }
  }

  protected def transformSpliceType(body: Tree, splice: Select)(using Context): Tree = {
    val body1 = transform(body)(using spliceContext)
    if ctx.reporter.hasErrors then
      splice
    else
      val tagRef = getQuoteTypeTags.getTagRef(splice.qualifier.tpe.asInstanceOf[TermRef])
      ref(tagRef).withSpan(splice.span)
  }

  def transformTypeAnnotationSplices(tp: Type)(using Context) = new TypeMap {
    def apply(tp: Type): Type = tp match
      case tp: AnnotatedType =>
        val newAnnotTree = transform(tp.annot.tree)
        derivedAnnotatedType(tp, apply(tp.parent), tp.annot.derivedAnnotation(newAnnotTree))
      case _ =>
        mapOver(tp)
  }.apply(tp)

  /** Check that annotations do not contain quotes and and that splices are valid */
  private def checkAnnotations(tree: Tree)(using Context): Unit =
    tree match
      case tree: DefTree =>
        lazy val annotCtx = ctx.fresh.setProperty(InAnnotation, true).withOwner(tree.symbol)
        for (annot <- tree.symbol.annotations) annot match
          case annot: BodyAnnotation => annot // already checked in PrepareInlineable before the creation of the BodyAnnotation
          case annot => transform(annot.tree)(using annotCtx)
      case _ =>

  /** Heal types in the info of the given tree */
  private def healInfo(tree: Tree, pos: SrcPos)(using Context): Unit =
    tree.symbol.info = healType(pos)(tree.symbol.info)

  /** If the type refers to a locally defined symbol (either directly, or in a pickled type),
   *  check that its staging level matches the current level.
   *  - Static types and term are allowed at any level.
   *  - If a type reference is used a higher level, then it is inconsistent.
   *    Will attempt to heal before failing.
   *  - If a term reference is used a higher level, then it is inconsistent.
   *    It cannot be healed because the term will not exist in any future stage.
   *
   *  If `T` is a reference to a type at the wrong level, try to heal it by replacing it with
   *  a type tag of type `quoted.Type[T]`.
   *  The tag is generated by an instance of `QuoteTypeTags` directly if the splice is explicit
   *  or indirectly by `tryHeal`.
   */
  protected def healType(pos: SrcPos)(tpe: Type)(using Context) =
    new HealType(pos).apply(tpe)

  /** Check level consistency of terms references */
  private def checkLevelConsistency(tree: Ident | This)(using Context): Unit =
    new TypeTraverser {
      def traverse(tp: Type): Unit =
        tp match
          case tp @ TermRef(NoPrefix, _) if !tp.symbol.isStatic && level != levelOf(tp.symbol) =>
            levelError(tp.symbol, tp, tree.srcPos)
          case tp: ThisType if level != -1 && level != levelOf(tp.cls) =>
            levelError(tp.cls, tp, tree.srcPos)
          case tp: AnnotatedType =>
            traverse(tp.parent)
          case _ if tp.typeSymbol.is(Package) =>
            // OK
          case _ =>
             traverseChildren(tp)
    }.traverse(tree.tpe)

  private def levelError(sym: Symbol, tp: Type, pos: SrcPos)(using Context): tp.type = {
    def symStr =
      if (!tp.isInstanceOf[ThisType]) sym.show
      else if (sym.is(ModuleClass)) sym.sourceModule.show
      else i"${sym.name}.this"
    val hint =
      if sym.is(Inline) && levelOf(sym) < level then
        "\n\n" +
        "Hint: Staged references to inline definition in quotes are only inlined after the quote is spliced into level 0 code by a macro. " +
        "Try moving this inline definition in a statically accessible location such as an object (this definition can be private)."
      else ""
    report.error(
      em"""access to $symStr from wrong staging level:
          | - the definition is at level ${levelOf(sym)},
          | - but the access is at level $level.$hint""", pos)
    tp
  }
}
