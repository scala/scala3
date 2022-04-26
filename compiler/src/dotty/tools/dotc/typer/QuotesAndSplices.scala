package dotty.tools.dotc
package typer

import dotty.tools.dotc.ast._
import dotty.tools.dotc.config.Feature._
import dotty.tools.dotc.config.SourceVersion._
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds.PatMatGivenVarName
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.typer.Implicits._
import dotty.tools.dotc.typer.Inferencing._
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.util.Stats.record

import scala.collection.mutable


/** Type quotes `'{ ... }` and splices `${ ... }` */
trait QuotesAndSplices {
  self: Typer =>

  import tpd._

  /** Translate `'{ e }` into `scala.quoted.Expr.apply(e)` and `'[T]` into `scala.quoted.Type.apply[T]`
   *  while tracking the quotation level in the context.
   */
  def typedQuote(tree: untpd.Quote, pt: Type)(using Context): Tree = {
    record("typedQuote")
    tree.quoted match {
      case untpd.Splice(innerExpr) if tree.isTerm && !ctx.mode.is(Mode.Pattern) =>
        report.warning("Canceled splice directly inside a quote. '{ ${ XYZ } } is equivalent to XYZ.", tree.srcPos)
      case untpd.TypSplice(innerType) if tree.isType =>
        report.warning("Canceled splice directly inside a quote. '[ ${ XYZ } ] is equivalent to XYZ.", tree.srcPos)
      case _ =>
    }
    val qctx = inferImplicitArg(defn.QuotesClass.typeRef, tree.span)

    if qctx.tpe.isInstanceOf[SearchFailureType] then
      report.error(missingArgMsg(qctx, defn.QuotesClass.typeRef, ""), ctx.source.atSpan(tree.span))
    else if !qctx.tpe.isStable then
      report.error(em"Quotes require stable Quotes, but found non stable $qctx", qctx.srcPos)

    if ctx.mode.is(Mode.Pattern) then
      typedQuotePattern(tree, pt, qctx).withSpan(tree.span)
    else if tree.quoted.isType then
      val msg = em"Consider using canonical type constructor scala.quoted.Type.of[${tree.quoted}] instead"
      if sourceVersion.isAtLeast(`future-migration`) then report.error(msg, tree.srcPos)
      else report.warning(msg, tree.srcPos)
      val typeOfTree = untpd.TypeApply(untpd.ref(defn.QuotedTypeModule_of.termRef), tree.quoted :: Nil).withSpan(tree.span)
      makeInlineable(typedTypeApply(typeOfTree, pt)(using quoteContext).select(nme.apply).appliedTo(qctx).withSpan(tree.span))
    else
      val exprQuoteTree = untpd.Apply(untpd.ref(defn.QuotedRuntime_exprQuote.termRef), tree.quoted)
      makeInlineable(typedApply(exprQuoteTree, pt)(using pushQuotes(qctx)).select(nme.apply).appliedTo(qctx).withSpan(tree.span))
  }

  private def makeInlineable(tree: Tree)(using Context): Tree =
    inContext(ctx.withOwner(ctx.owner.skipLocalOwners)) {
      PrepareInlineable.makeInlineable(tree)
    }

  /** Translate `${ t: Expr[T] }` into expression `t.splice` while tracking the quotation level in the context */
  def typedSplice(tree: untpd.Splice, pt: Type)(using Context): Tree = {
    record("typedSplice")
    checkSpliceOutsideQuote(tree)
    tree.expr match {
      case untpd.Quote(innerExpr) if innerExpr.isTerm =>
        report.warning("Canceled quote directly inside a splice. ${ '{ XYZ } } is equivalent to XYZ.", tree.srcPos)
      case _ =>
    }
    if (ctx.mode.is(Mode.QuotedPattern))
      if (isFullyDefined(pt, ForceDegree.flipBottom)) {
        def spliceOwner(ctx: Context): Symbol =
          if (ctx.mode.is(Mode.QuotedPattern)) spliceOwner(ctx.outer) else ctx.owner
        val pat = typedPattern(tree.expr, defn.QuotedExprClass.typeRef.appliedTo(pt))(
          using spliceContext.retractMode(Mode.QuotedPattern).addMode(Mode.Pattern).withOwner(spliceOwner(ctx)))
        val baseType = pat.tpe.baseType(defn.QuotedExprClass)
        val argType = if baseType != NoType then baseType.argTypesHi.head else defn.NothingType
        ref(defn.QuotedRuntime_exprSplice).appliedToType(argType).appliedTo(pat)
      }
      else {
        report.error(i"Type must be fully defined.\nConsider annotating the splice using a type ascription:\n  ($tree: XYZ).", tree.expr.srcPos)
        tree.withType(UnspecifiedErrorType)
      }
    else {
      if (StagingContext.level == 0) {
        // Mark the first inline method from the context as a macro
        def markAsMacro(c: Context): Unit =
          if (c.owner eq c.outer.owner) markAsMacro(c.outer)
          else if (c.owner.isInlineMethod) c.owner.setFlag(Macro)
          else if (!c.outer.owner.is(Package)) markAsMacro(c.outer)
          else assert(ctx.reporter.hasErrors) // Did not find inline def to mark as macro
        markAsMacro(ctx)
      }

      val (outerQctx, ctx1) = popQuotes()

      val internalSplice =
        outerQctx match
          case Some(qctxRef) => untpd.Apply(untpd.Apply(untpd.ref(defn.QuotedRuntime_exprNestedSplice.termRef), qctxRef), tree.expr)
          case _ => untpd.Apply(untpd.ref(defn.QuotedRuntime_exprSplice.termRef), tree.expr)

      typedApply(internalSplice, pt)(using ctx1).withSpan(tree.span)
    }
  }

  /** Types a splice applied to some arguments `$f(arg1, ..., argn)` in a quote pattern.
   *
   *  The tree is desugared into `$f.apply(arg1, ..., argn)` where the expression `$f`
   *  is expected to type as a function type `(T1, ..., Tn) => R`.
   *  `Ti` is the type of the argument `argi` and R if the type of the prototype.
   *  The prototype must be fully defined to be able to infer the type of `R`.
   */
  def typedAppliedSplice(tree: untpd.Apply, pt: Type)(using Context): Tree = {
    assert(ctx.mode.is(Mode.QuotedPattern))
    val untpd.Apply(splice: untpd.Splice, args) = tree
    if !isFullyDefined(pt, ForceDegree.flipBottom) then
      report.error(i"Type must be fully defined.", splice.srcPos)
      tree.withType(UnspecifiedErrorType)
    else if splice.isInBraces then // ${x}(...) match an application
      val typedArgs = args.map(arg => typedExpr(arg))
      val argTypes = typedArgs.map(_.tpe.widenTermRefExpr)
      val splice1 = typedSplice(splice, defn.FunctionOf(argTypes, pt))
      Apply(splice1.select(nme.apply), typedArgs).withType(pt).withSpan(tree.span)
    else // $x(...) higher-order quasipattern
      val typedArgs = args.map {
        case arg: untpd.Ident =>
          typedExpr(arg)
        case arg =>
          report.error("Open patttern exprected an identifier", arg.srcPos)
          EmptyTree
      }
      if args.isEmpty then
        report.error("Missing arguments for open pattern", tree.srcPos)
      val argTypes = typedArgs.map(_.tpe.widenTermRefExpr)
      val typedPat = typedSplice(splice, defn.FunctionOf(argTypes, pt))
      ref(defn.QuotedRuntimePatterns_patternHigherOrderHole).appliedToType(pt).appliedTo(typedPat, SeqLiteral(typedArgs, TypeTree(defn.AnyType)))
  }

  /** Translate ${ t: Type[T] }` into type `t.splice` while tracking the quotation level in the context */
  def typedTypSplice(tree: untpd.TypSplice, pt: Type)(using Context): Tree = {
    record("typedTypSplice")
    checkSpliceOutsideQuote(tree)
    tree.expr match {
      case untpd.Quote(innerType) if innerType.isType =>
        report.warning("Canceled quote directly inside a splice. ${ '[ XYZ ] } is equivalent to XYZ.", tree.srcPos)
      case _ =>
    }

    if ctx.mode.is(Mode.QuotedPattern) && level == 1 then
      report.error(
            """`$` for quote pattern variable is not supported anymore.
               |Use lower cased variable name without the `$` instead.""".stripMargin,
            tree.srcPos)
      ref(defn.NothingType)
    else
      val tree1 = typedSelect(untpd.Select(tree.expr, tpnme.Underlying), pt)(using spliceContext).withSpan(tree.span)
      val msg = em"Consider using canonical type reference ${tree1.tpe} instead"
      if sourceVersion.isAtLeast(`future-migration`) then report.error(msg, tree.srcPos)
      else report.warning(msg, tree.srcPos)
      tree1
  }

  /** Type a pattern variable name `t` in quote pattern as `${given t$giveni: Type[t @ _]}`.
   *  The resulting pattern is the split in `splitQuotePattern`.
   */
  def typedQuotedTypeVar(tree: untpd.Ident, pt: Type)(using Context): Tree =
    def spliceOwner(ctx: Context): Symbol =
      if (ctx.mode.is(Mode.QuotedPattern)) spliceOwner(ctx.outer) else ctx.owner
    val name = tree.name.toTypeName
    val nameOfSyntheticGiven = PatMatGivenVarName.fresh(tree.name.toTermName)
    val expr = untpd.cpy.Ident(tree)(nameOfSyntheticGiven)
    val typeSymInfo = pt match
      case pt: TypeBounds => pt
      case _ => TypeBounds.empty
    val typeSym = newSymbol(spliceOwner(ctx), name, EmptyFlags, typeSymInfo, NoSymbol, tree.span)
    typeSym.addAnnotation(Annotation(New(ref(defn.QuotedRuntimePatterns_patternTypeAnnot.typeRef)).withSpan(tree.span)))
    val pat = typedPattern(expr, defn.QuotedTypeClass.typeRef.appliedTo(typeSym.typeRef))(
        using spliceContext.retractMode(Mode.QuotedPattern).withOwner(spliceOwner(ctx)))
    pat.select(tpnme.Underlying)

  def typedHole(tree: untpd.Hole, pt: Type)(using Context): Tree =
    val tpt = typedType(tree.tpt)
    assignType(tree, tpt)

  private def checkSpliceOutsideQuote(tree: untpd.Tree)(using Context): Unit =
    if (level == 0 && !ctx.owner.ownersIterator.exists(_.is(Inline)))
      report.error("Splice ${...} outside quotes '{...} or inline method", tree.srcPos)
    else if (level < 0)
      report.error(
        s"""Splice $${...} at level $level.
          |
          |Inline method may contain a splice at level 0 but the contents of this splice cannot have a splice.
          |""".stripMargin, tree.srcPos
      )

  /** Split a typed quoted pattern is split into its type bindings, pattern expression and inner patterns.
   *  Type definitions with `@patternType` will be inserted in the pattern expression for each type binding.
   *
   *  A quote pattern
   *  ```
   *  case '{ type ${given t$giveni: Type[t @ _]}; ${ls: Expr[List[t]]} } => ...
   *  ```
   *  will return
   *  ```
   *  (
   *    Map(<t$giveni>: Symbol -> <t @ _>: Bind),
   *    <'{
   *       @scala.internal.Quoted.patternType type t
   *       scala.internal.Quoted.patternHole[List[t]]
   *    }>: Tree,
   *    List(<ls: Expr[List[t]]>: Tree)
   *  )
   *  ```
   */
  private def splitQuotePattern(quoted: Tree)(using Context): (Map[Symbol, Bind], Tree, List[Tree]) = {
    val ctx0 = ctx

    val typeBindings: collection.mutable.Map[Symbol, Bind] = collection.mutable.Map.empty
    def getBinding(sym: Symbol): Bind =
      typeBindings.getOrElseUpdate(sym, {
        val bindingBounds = sym.info
        val bsym = newPatternBoundSymbol(sym.name.toString.stripPrefix("$").toTypeName, bindingBounds, quoted.span)
        Bind(bsym, untpd.Ident(nme.WILDCARD).withType(bindingBounds)).withSpan(quoted.span)
      })

    object splitter extends tpd.TreeMap {
      private var variance: Int = 1

      inline private def atVariance[T](v: Int)(op: => T): T = {
        val saved = variance
        variance = v
        val res = op
        variance = saved
        res
      }

      val patBuf = new mutable.ListBuffer[Tree]
      val freshTypePatBuf = new mutable.ListBuffer[Tree]
      val freshTypeBindingsBuff = new mutable.ListBuffer[Tree]
      val typePatBuf = new mutable.ListBuffer[Tree]
      override def transform(tree: Tree)(using Context) = tree match {
        case Typed(Apply(fn, pat :: Nil), tpt) if fn.symbol.isExprSplice && !tpt.tpe.derivesFrom(defn.RepeatedParamClass) =>
          val tpt1 = transform(tpt) // Transform type bindings
          val exprTpt = AppliedTypeTree(TypeTree(defn.QuotedExprClass.typeRef), tpt1 :: Nil)
          val newSplice = ref(defn.QuotedRuntime_exprSplice).appliedToType(tpt1.tpe).appliedTo(Typed(pat, exprTpt))
          transform(newSplice)
        case Apply(TypeApply(fn, targs), Apply(sp, pat :: Nil) :: args :: Nil) if fn.symbol == defn.QuotedRuntimePatterns_patternHigherOrderHole =>
          args match // TODO support these patterns. Possibly using scala.quoted.util.Var
            case SeqLiteral(args, _) =>
              for arg <- args; if arg.symbol.is(Mutable) do
                report.error("References to `var`s cannot be used in higher-order pattern", arg.srcPos)
          try ref(defn.QuotedRuntimePatterns_higherOrderHole.termRef).appliedToTypeTrees(targs).appliedTo(args).withSpan(tree.span)
          finally {
            val patType = pat.tpe.widen
            val patType1 = patType.translateFromRepeated(toArray = false)
            val pat1 = if (patType eq patType1) pat else pat.withType(patType1)
            patBuf += pat1
          }
        case Apply(fn, pat :: Nil) if fn.symbol.isExprSplice =>
          try ref(defn.QuotedRuntimePatterns_patternHole.termRef).appliedToType(tree.tpe).withSpan(tree.span)
          finally {
            val patType = pat.tpe.widen
            val patType1 = patType.translateFromRepeated(toArray = false)
            val pat1 = if (patType eq patType1) pat else pat.withType(patType1)
            patBuf += pat1
          }
        case Select(pat, _) if tree.symbol.isTypeSplice =>
          val sym = tree.tpe.dealias.typeSymbol
          if sym.exists then
            val tdef = TypeDef(sym.asType).withSpan(sym.span)
            val nameOfSyntheticGiven = pat.symbol.name.toTermName
            freshTypeBindingsBuff += transformTypeBindingTypeDef(nameOfSyntheticGiven, tdef, freshTypePatBuf)
            TypeTree(tree.tpe.dealias).withSpan(tree.span)
          else
            tree
        case tdef: TypeDef  =>
          if tdef.symbol.hasAnnotation(defn.QuotedRuntimePatterns_patternTypeAnnot) then
            transformTypeBindingTypeDef(PatMatGivenVarName.fresh(tdef.name.toTermName), tdef, typePatBuf)
          else if tdef.symbol.isClass then
            val kind = if tdef.symbol.is(Module) then "objects" else "classes"
            report.error("Implementation restriction: cannot match " + kind, tree.srcPos)
            EmptyTree
          else
            super.transform(tree)
        case tree @ AppliedTypeTree(tpt, args) =>
            val args1: List[Tree] = args.zipWithConserve(tpt.tpe.typeParams.map(_.paramVarianceSign)) { (arg, v) =>
              arg.tpe match {
                case _: TypeBounds => transform(arg)
                case _ => atVariance(variance * v)(transform(arg))
              }
            }
            cpy.AppliedTypeTree(tree)(transform(tpt), args1)
        case tree: NamedDefTree =>
          if tree.name.is(NameKinds.WildcardParamName) then
            report.warning(
              "Use of `_` for lambda in quoted pattern. Use explicit lambda instead or use `$_` to match any term.",
              tree.srcPos)
          if tree.name.isTermName && !tree.nameSpan.isSynthetic && tree.name.startsWith("$") then
            report.error("Names cannot start with $ quote pattern ", tree.namePos)
          super.transform(tree)
        case _: Match =>
          report.error("Implementation restriction: cannot match `match` expressions", tree.srcPos)
          EmptyTree
        case _: Try =>
          report.error("Implementation restriction: cannot match `try` expressions", tree.srcPos)
          EmptyTree
        case _: Return =>
          report.error("Implementation restriction: cannot match `return` statements", tree.srcPos)
          EmptyTree
        case _ =>
          super.transform(tree)
      }

      private def transformTypeBindingTypeDef(nameOfSyntheticGiven: TermName, tdef: TypeDef, buff: mutable.Builder[Tree, List[Tree]])(using Context): Tree = {
        if (variance == -1)
          tdef.symbol.addAnnotation(Annotation(New(ref(defn.QuotedRuntimePatterns_fromAboveAnnot.typeRef)).withSpan(tdef.span)))
        val bindingType = getBinding(tdef.symbol).symbol.typeRef
        val bindingTypeTpe = AppliedType(defn.QuotedTypeClass.typeRef, bindingType :: Nil)
        val sym = newPatternBoundSymbol(nameOfSyntheticGiven, bindingTypeTpe, tdef.span, flags = ImplicitVal)(using ctx0)
        buff += Bind(sym, untpd.Ident(nme.WILDCARD).withType(bindingTypeTpe)).withSpan(tdef.span)
        super.transform(tdef)
      }
    }
    val shape0 = splitter.transform(quoted)
    val patterns = (splitter.freshTypePatBuf.iterator ++ splitter.typePatBuf.iterator ++ splitter.patBuf.iterator).toList
    val freshTypeBindings = splitter.freshTypeBindingsBuff.result()

    val shape1 = seq(
      freshTypeBindings,
      shape0
    )
    val shape2 =
      if (freshTypeBindings.isEmpty) shape1
      else {
        val isFreshTypeBindings = freshTypeBindings.map(_.symbol).toSet
        val typeMap = new TypeMap() {
          def apply(tp: Type): Type = tp match {
            case tp: TypeRef if tp.symbol.isTypeSplice =>
              val tp1 = tp.dealias
              if (isFreshTypeBindings(tp1.typeSymbol)) tp1
              else tp
            case tp => mapOver(tp)
          }
        }
        new TreeTypeMap(typeMap = typeMap).transform(shape1)
      }

    (typeBindings.toMap, shape2, patterns)
  }

  /** Type a quote pattern `case '{ <quoted> } =>` qiven the a current prototype. Typing the pattern
   *  will also transform it into a call to `scala.internal.quoted.Expr.unapply`.
   *
   *  Code directly inside the quote is typed as an expression using Mode.QuotedPattern. Splices
   *  within the quotes become patterns again and typed accordingly.
   *
   *  ```
   *  case '{ ($ls: List[t]) } =>
   *    // `t$giveni` is of type `Type[t]` for some unknown `t`
   *    // `t$giveni` is implicitly available
   *    // `ls` is of type `Expr[List[t]]`
   *    '{ val h: $t = $ls.head  }
   *  ```
   *
   *  For each type splice we will create a new type binding in the pattern match (`t @ _` in this case)
   *  and a corresponding type in the quoted pattern as a hole (`@patternType type t` in this case).
   *  All these generated types are inserted at the start of the quoted code.
   *
   *  After typing the tree will resemble
   *
   *  ```
   *  case '{ type ${given t$giveni: Type[t @ _]}; ${ls: Expr[List[t]]} } => ...
   *  ```
   *
   *  Then the pattern is _split_ into the expression contained in the pattern replacing the splices by holes,
   *  and the patterns in the splices. All these are recombined into a call to `Matcher.unapply`.
   *
   *  ```
   *  case scala.internal.quoted.Expr.unapply[
   *          Tuple1[t @ _], // Type binging definition
   *          Tuple2[Type[t], Expr[List[t]]] // Typing the result of the pattern match
   *        ](
   *          Tuple2.unapply
   *            [Type[t], Expr[List[t]]] //Propagated from the tuple above
   *            (given t$giveni @ _, ls @ _: Expr[List[t]]) // from the spliced patterns
   *        )(
   *         '{ // Runtime quote Matcher.unapply uses to mach against. Expression directly inside the quoted pattern without the splices
   *            @scala.internal.Quoted.patternType type t
   *            scala.internal.Quoted.patternHole[List[t]]
   *          },
   *          true, // If there is at least one type splice. Used to instantiate the context with or without GADT constraints
   *          x$2 // tasty.Reflection instance
   *        ) => ...
   *  ```
   */
  private def typedQuotePattern(tree: untpd.Quote, pt: Type, qctx: Tree)(using Context): Tree = {
    if tree.quoted.isTerm && !pt.derivesFrom(defn.QuotedExprClass) then
      report.error("Quote pattern can only match scrutinees of type scala.quoted.Expr", tree.srcPos)
    else if tree.quoted.isType && !pt.derivesFrom(defn.QuotedTypeClass) then
      report.error("Quote pattern can only match scrutinees of type scala.quoted.Type", tree.srcPos)

    val quoted = tree.quoted
    val exprPt = pt.baseType(if quoted.isType then defn.QuotedTypeClass else defn.QuotedExprClass)
    val quotedPt = exprPt.argInfos.headOption match {
      case Some(argPt: ValueType) => argPt // excludes TypeBounds
      case _ => defn.AnyType
    }
    val quoted0 = desugar.quotedPattern(quoted, untpd.TypedSplice(TypeTree(quotedPt)))
    val quoteCtx = quoteContext.addMode(Mode.QuotedPattern).retractMode(Mode.Pattern)
    val quoted1 =
      if quoted.isType then typedType(quoted0, WildcardType)(using quoteCtx)
      else typedExpr(quoted0, WildcardType)(using quoteCtx)

    val (typeBindings, shape, splices) = splitQuotePattern(quoted1)

    class ReplaceBindings extends TypeMap() {
      override def apply(tp: Type): Type = tp match {
        case tp: TypeRef =>
          val tp1 = if (tp.symbol.isTypeSplice) tp.dealias else tp
          mapOver(typeBindings.get(tp1.typeSymbol).fold(tp)(_.symbol.typeRef))
        case tp => mapOver(tp)
      }
    }
    val replaceBindings = new ReplaceBindings
    val patType = defn.tupleType(splices.tpes.map(tpe => replaceBindings(tpe.widen)))

    val typeBindingsTuple = tpd.tupleTypeTree(typeBindings.values.toList)

    val replaceBindingsInTree = new TreeMap {
      private var bindMap = Map.empty[Symbol, Symbol]
      override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
        tree match {
          case tree: Bind =>
            val sym = tree.symbol
            val newInfo = replaceBindings(sym.info)
            val newSym = newSymbol(sym.owner, sym.name, sym.flags, newInfo, sym.privateWithin, sym.coord)
            bindMap += sym -> newSym
            Bind(newSym, transform(tree.body)).withSpan(sym.span)
          case _ =>
            super.transform(tree).withType(replaceBindingsInType(tree.tpe))
        }
      private val replaceBindingsInType = new ReplaceBindings {
        override def apply(tp: Type): Type = tp match {
          case tp: TermRef => bindMap.get(tp.termSymbol).fold[Type](tp)(_.typeRef)
          case tp => super.apply(tp)
        }
      }
    }

    val splicePat =
      if splices.isEmpty then ref(defn.EmptyTupleModule.termRef)
      else typed(untpd.Tuple(splices.map(x => untpd.TypedSplice(replaceBindingsInTree.transform(x)))).withSpan(quoted.span), patType)

    val quoteClass = if (tree.quoted.isTerm) defn.QuotedExprClass else defn.QuotedTypeClass
    val quotedPattern =
      if (tree.quoted.isTerm) ref(defn.QuotedRuntime_exprQuote.termRef).appliedToType(defn.AnyType).appliedTo(shape).select(nme.apply).appliedTo(qctx)
      else ref(defn.QuotedTypeModule_of.termRef).appliedToTypeTree(shape).appliedTo(qctx)

    val matchModule = if tree.quoted.isTerm then defn.QuoteMatching_ExprMatch else defn.QuoteMatching_TypeMatch
    val unapplyFun = qctx.asInstance(defn.QuoteMatchingClass.typeRef).select(matchModule).select(nme.unapply)

    UnApply(
      fun = unapplyFun.appliedToTypeTrees(typeBindingsTuple :: TypeTree(patType) :: Nil),
      implicits = quotedPattern :: Nil,
      patterns = splicePat :: Nil,
      proto = quoteClass.typeRef.appliedTo(replaceBindings(quoted1.tpe) & quotedPt))
  }
}
