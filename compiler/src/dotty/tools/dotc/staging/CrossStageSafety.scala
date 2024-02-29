package dotty.tools.dotc
package staging

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Annotations.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.quoted.QuotePatterns
import dotty.tools.dotc.staging.QuoteTypeTags.*
import dotty.tools.dotc.staging.StagingLevel.*
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.util.Spans.*
import dotty.tools.dotc.util.SrcPos

/** Checks that staging level consistency holds and heals staged types.
 *
 *  Local term references are level consistent if and only if they are used at the same level as their definition.
 *
 *  Local type references can be used at the level of their definition or lower. If used used at a higher level,
 *  it will be healed if possible, otherwise it is inconsistent.
 *
 *  Healing a type consists in replacing locally defined types defined at staging level 0 and used in higher levels.
 *  For each type local `T` that is defined at level 0 and used in a quote, we summon a tag `t: Type[T]`. This `t`
 *  tag must be defined at level 0. The tags will be listed in the `tags` of the level 0 quote (`'<t>{ ... }`) and
 *  each reference to `T` will be replaced by `t.Underlying` in the body of the quote.
 *
 *  We delay the healing of types in quotes at level 1 or higher until those quotes reach level 0. At this point
 *  more types will be statically known and fewer types will need to be healed. This also keeps the nested quotes
 *  in their original form, we do not want macro users to see any artifacts of this phase in quoted expressions
 *  they might inspect.
 *
 *  Type heal example:
 *
 *    '{
 *      val x: List[T] = List[T]()
 *      '{ .. T .. }
 *      ()
 *    }
 *
 *  is transformed to
 *
 *    '<t>{ // where `t` is a given term of type `Type[T]`
 *      val x: List[t.Underlying] = List[t.Underlying]();
 *      '{ .. t.Underlying .. }
 *      ()
 *     }
 *
 */
class CrossStageSafety extends TreeMapWithStages {
  import tpd.*

  private val InAnnotation = Property.Key[Unit]()

  override def transform(tree: Tree)(using Context): Tree =
    if (tree.source != ctx.source && tree.source.exists)
      transform(tree)(using ctx.withSource(tree.source))
    else tree match
      case CancelledQuote(tree) =>
        transform(tree) // Optimization: `'{ $x }` --> `x`
      case tree: Quote =>
        if (ctx.property(InAnnotation).isDefined)
          report.error("Cannot have a quote in an annotation", tree.srcPos)

        val tree1 =
          val stripAnnotationsDeep: TypeMap = new TypeMap:
            def apply(tp: Type): Type = mapOver(tp.stripAnnots)
          val bodyType1 = healType(tree.srcPos)(stripAnnotationsDeep(tree.bodyType))
          tree.withBodyType(bodyType1)

        if level == 0 then
          val (tags, body1) = inContextWithQuoteTypeTags { transform(tree1.body)(using quoteContext) }
          cpy.Quote(tree1)(body1, tags)
        else
          super.transform(tree1)

      case CancelledSplice(tree) =>
        transform(tree) // Optimization: `${ 'x }` --> `x`
      case tree: Splice =>
        val body1 = transform(tree.expr)(using spliceContext)
        val tpe1 =
          if level == 0 then tree.tpe
          else healType(tree.srcPos)(tree.tpe.widenTermRefExpr)
        untpd.cpy.Splice(tree)(body1).withType(tpe1)

      case tree @ QuotedTypeOf(body) =>
        if (ctx.property(InAnnotation).isDefined)
          report.error("Cannot have a quote in an annotation", tree.srcPos)

        if level == 0 then
          val (tags, body1) = inContextWithQuoteTypeTags { transform(body)(using quoteContext) }
          val quotes = transform(tree.args.head)
          tags match
            case tag :: Nil if body1.isType && body1.tpe =:= tag.tpe.select(tpnme.Underlying) =>
              tag // Optimization: `quoted.Type.of[x.Underlying](quotes)`  -->  `x`
            case _ =>
              // `quoted.Type.of[<body>](<quotes>)` --> `'[<body1>].apply(<quotes>)`
              tpd.Quote(body1, tags).select(nme.apply).appliedTo(quotes).withSpan(tree.span)
        else
          super.transform(tree)
      case _: DefDef if tree.symbol.isInlineMethod =>
        tree

      case tree: CaseDef if level == 0 =>
        val pat1 = new TreeMap {
          // Encode all quote patterns to materialize the given `Type[ti]` bindings
          // for each type binding `ti` of the quote pattern. These will be summoned
          // by HealType in the right hand side of the case definition.
          override def transform(tree: tpd.Tree)(using Context): tpd.Tree = tree match
            case tree: QuotePattern if level == 0 =>
              super.transform(QuotePatterns.encode(tree))
            case tree => super.transform(tree)
        }.transform(tree.pat)
        val tree1 = cpy.CaseDef(tree)(pat1, tree.guard, tree.body)
        super.transform(tree1)

      case _ if !inQuoteOrSpliceScope =>
        checkAnnotations(tree) // Check quotes in annotations
        super.transform(tree)

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
  end transform

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
      else if level > 0 && sym.info.derivesFrom(defn.QuotesClass) then
        s"""\n
         |Hint: Nested quote needs a local context defined at level $level.
         |One way to introduce this context is to give the outer quote the type `Expr[Quotes ?=> Expr[T]]`.""".stripMargin
      else ""
    report.error(
      em"""access to $symStr from wrong staging level:
          | - the definition is at level ${levelOf(sym)},
          | - but the access is at level $level.$hint""", pos)
    tp
  }

  private object CancelledQuote:
    def unapply(tree: Quote): Option[Tree] =
      def rec(tree: Tree): Option[Tree] = tree match
        case Block(Nil, expr) => rec(expr)
        case Splice(inner) => Some(inner)
        case _ => None
      rec(tree.body)

  private object CancelledSplice:
    def unapply(tree: Splice): Option[Tree] =
      def rec(tree: Tree): Option[Tree] = tree match
        case Block(Nil, expr) => rec(expr)
        case Quote(inner, _) => Some(inner)
        case _ => None
      rec(tree.expr)
}
