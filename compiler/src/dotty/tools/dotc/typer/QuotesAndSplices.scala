package dotty.tools.dotc.typer

import dotty.tools.dotc.ast._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds.UniqueName
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.reporting._
import dotty.tools.dotc.typer.Implicits._
import dotty.tools.dotc.typer.Inferencing._
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.util.Stats.record

import scala.collection.mutable

import scala.annotation.tailrec
import scala.annotation.internal.sharable
import scala.annotation.threadUnsafe

/** Type quotes `'{ ... }` and splices `${ ... }` */
trait QuotesAndSplices {
  self: Typer =>

  import tpd._

  /** Translate `'{ t }` into `scala.quoted.Expr.apply(t)` and `'[T]` into `scala.quoted.Type.apply[T]`
   *  while tracking the quotation level in the context.
   */
  def typedQuote(tree: untpd.Quote, pt: Type)(implicit ctx: Context): Tree = {
    record("typedQuote")
    ctx.compilationUnit.needsStaging = true
    tree.quoted match {
      case untpd.Splice(innerExpr) if tree.isTerm =>
        ctx.warning("Canceled splice directly inside a quote. '{ ${ XYZ } } is equivalent to XYZ.", tree.sourcePos)
      case untpd.TypSplice(innerType) if tree.isType =>
        ctx.warning("Canceled splice directly inside a quote. '[ ${ XYZ } ] is equivalent to XYZ.", tree.sourcePos)
      case _ =>
    }
    val tree1 =
      if (ctx.mode.is(Mode.Pattern) && level == 0) typedQuotePattern(tree, pt)
      else if (tree.quoted.isType) typedTypeApply(untpd.TypeApply(untpd.ref(defn.InternalQuoted_typeQuote.termRef), tree.quoted :: Nil), pt)(quoteContext)
      else typedApply(untpd.Apply(untpd.ref(defn.InternalQuoted_exprQuote.termRef), tree.quoted), pt)(quoteContext)
    tree1.withSpan(tree.span)
  }

  /** Translate `${ t: Expr[T] }` into expression `t.splice` while tracking the quotation level in the context */
  def typedSplice(tree: untpd.Splice, pt: Type)(implicit ctx: Context): Tree = {
    record("typedSplice")
    ctx.compilationUnit.needsStaging = true
    checkSpliceOutsideQuote(tree)
    tree.expr match {
      case untpd.Quote(innerExpr) if innerExpr.isTerm =>
        ctx.warning("Canceled quote directly inside a splice. ${ '{ XYZ } } is equivalent to XYZ.", tree.sourcePos)
      case _ =>
    }
    if (ctx.mode.is(Mode.QuotedPattern) && level == 1)
      if (isFullyDefined(pt, ForceDegree.all)) {
        def spliceOwner(ctx: Context): Symbol =
          if (ctx.mode.is(Mode.QuotedPattern)) spliceOwner(ctx.outer) else ctx.owner
        val pat = typedPattern(tree.expr, defn.QuotedExprClass.typeRef.appliedTo(pt))(
          spliceContext.retractMode(Mode.QuotedPattern).withOwner(spliceOwner(ctx)))
        Splice(pat)
      }
      else {
        ctx.error(i"Type must be fully defined.\nConsider annotating the splice using a type ascription:\n  ($tree: XYZ).", tree.expr.sourcePos)
        tree.withType(UnspecifiedErrorType)
      }
    else {
      if (StagingContext.level == 0) {
        // Mark the first inline method from the context as a macro
        def markAsMacro(c: Context): Unit =
          if (c.owner eq c.outer.owner) markAsMacro(c.outer)
          else if (c.owner.isInlineMethod) c.owner.setFlag(Macro)
          else if (!c.outer.owner.is(Package)) markAsMacro(c.outer)
        markAsMacro(ctx)
      }
      typedApply(untpd.Apply(untpd.ref(defn.InternalQuoted_exprSplice.termRef), tree.expr), pt)(spliceContext).withSpan(tree.span)
    }
  }

  /** Translate ${ t: Type[T] }` into type `t.splice` while tracking the quotation level in the context */
  def typedTypSplice(tree: untpd.TypSplice, pt: Type)(implicit ctx: Context): Tree = {
    record("typedTypSplice")
    ctx.compilationUnit.needsStaging = true
    checkSpliceOutsideQuote(tree)
    tree.expr match {
      case untpd.Quote(innerType) if innerType.isType =>
        ctx.warning("Canceled quote directly inside a splice. ${ '[ XYZ ] } is equivalent to XYZ.", tree.sourcePos)
      case _ =>
    }
    if (ctx.mode.is(Mode.QuotedPattern) && level == 1)
      if (isFullyDefined(pt, ForceDegree.all)) {
        ctx.error(i"Spliced type pattern must not be fully defined. Consider using $pt directly", tree.expr.sourcePos)
        tree.withType(UnspecifiedErrorType)
      }
      else {
        def spliceOwner(ctx: Context): Symbol =
          if (ctx.mode.is(Mode.QuotedPattern)) spliceOwner(ctx.outer) else ctx.owner
        val name = tree.expr match {
          case Ident(name) => ("$" + name).toTypeName
          case Typed(Ident(name), _) => ("$" + name).toTypeName
          case Bind(name, _) => ("$" + name).toTypeName
          case _ => NameKinds.UniqueName.fresh("$".toTypeName)
        }
        val typeSym = ctx.newSymbol(spliceOwner(ctx), name, EmptyFlags, TypeBounds.empty, NoSymbol, tree.expr.span)
        typeSym.addAnnotation(Annotation(New(ref(defn.InternalQuoted_patternBindHoleAnnot.typeRef)).withSpan(tree.expr.span)))
        val pat = typedPattern(tree.expr, defn.QuotedTypeClass.typeRef.appliedTo(typeSym.typeRef))(
            spliceContext.retractMode(Mode.QuotedPattern).withOwner(spliceOwner(ctx)))
        pat.select(tpnme.splice)
      }
    else
      typedSelect(untpd.Select(tree.expr, tpnme.splice), pt)(spliceContext).withSpan(tree.span)
  }

  private def checkSpliceOutsideQuote(tree: untpd.Tree)(implicit ctx: Context): Unit =
    if (level == 0 && !ctx.owner.ownersIterator.exists(_.is(Inline)))
      ctx.error("Splice ${...} outside quotes '{...} or inline method", tree.sourcePos)
    else if (level < 0)
      ctx.error(
        s"""Splice $${...} at level $level.
          |
          |Inline method may contain a splice at level 0 but the contents of this splice cannot have a splice.
          |""".stripMargin, tree.sourcePos
      )

  /** Split a typed quoted pattern is split into its type bindings, pattern expression and inner patterns.
   *  Type definitions with `@patternBindHole` will be inserted in the pattern expression for each type binding.
   *
   *  A quote pattern
   *  ```
   *  case '{ type ${given t: Type[$t @ _]}; ${ls: Expr[List[$t]]} } => ...
   *  ```
   *  will return
   *  ```
   *  (
   *    Map(<$t>: Symbol -> <$t @ _>: Bind),
   *    <'{
   *       @scala.internal.Quoted.patternBindHole type $t
   *       scala.internal.Quoted.patternHole[List[$t]]
   *    }>: Tree,
   *    List(<ls: Expr[List[$t]]>: Tree)
   *  )
   *  ```
   */
  private def splitQuotePattern(quoted: Tree)(implicit ctx: Context): (Map[Symbol, Bind], Tree, List[Tree]) = {
    val ctx0 = ctx

    val typeBindings: collection.mutable.Map[Symbol, Bind] = collection.mutable.Map.empty
    def getBinding(sym: Symbol): Bind =
      typeBindings.getOrElseUpdate(sym, {
        val bindingBounds = sym.info
        val bsym = ctx.newPatternBoundSymbol(sym.name.toTypeName, bindingBounds, quoted.span)
        Bind(bsym, untpd.Ident(nme.WILDCARD).withType(bindingBounds)).withSpan(quoted.span)
      })

    object splitter extends tpd.TreeMap {
      private var variance: Int = 1

      @forceInline private def atVariance[T](v: Int)(op: => T): T = {
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
      override def transform(tree: Tree)(implicit ctx: Context) = tree match {
        case Typed(Splice(pat), tpt) if !tpt.tpe.derivesFrom(defn.RepeatedParamClass) =>
          val tpt1 = transform(tpt) // Transform type bindings
          val exprTpt = AppliedTypeTree(TypeTree(defn.QuotedExprClass.typeRef), tpt1 :: Nil)
          transform(Splice(Typed(pat, exprTpt)))
        case Splice(pat) =>
          try ref(defn.InternalQuoted_patternHole.termRef).appliedToType(tree.tpe).withSpan(tree.span)
          finally {
            val patType = pat.tpe.widen
            val patType1 = patType.underlyingIfRepeated(isJava = false)
            val pat1 = if (patType eq patType1) pat else pat.withType(patType1)
            patBuf += pat1
          }
        case Select(pat, _) if tree.symbol == defn.QuotedType_splice =>
          val sym = tree.tpe.dealias.typeSymbol.asType
          val tdef = TypeDef(sym).withSpan(sym.span)
          freshTypeBindingsBuff += transformTypeBindingTypeDef(tdef, freshTypePatBuf)
          TypeTree(tree.tpe.dealias).withSpan(tree.span)

        case ddef: ValOrDefDef =>
          if (ddef.symbol.hasAnnotation(defn.InternalQuoted_patternBindHoleAnnot)) {
            val bindingType = ddef.symbol.info match {
              case t: ExprType => t.resType
              case t: MethodType => t.toFunctionType()
              case t: PolyType =>
                HKTypeLambda(t.paramNames)(
                    x => t.paramInfos.mapConserve(_.subst(t, x).asInstanceOf[TypeBounds]),
                    x => t.resType.subst(t, x).toFunctionType())
              case t => t
            }
            val bindingExprTpe = AppliedType(defn.QuotedMatchingBindingClass.typeRef, bindingType :: Nil)
            assert(ddef.name.startsWith("$"))
            val bindName = ddef.name.toString.stripPrefix("$").toTermName
            val sym = ctx0.newPatternBoundSymbol(bindName, bindingExprTpe, ddef.span)
            patBuf += Bind(sym, untpd.Ident(nme.WILDCARD).withType(bindingExprTpe)).withSpan(ddef.span)
          }
          super.transform(tree)
        case tdef: TypeDef if tdef.symbol.hasAnnotation(defn.InternalQuoted_patternBindHoleAnnot) =>
          transformTypeBindingTypeDef(tdef, typePatBuf)
        case tree @ AppliedTypeTree(tpt, args) =>
            val args1: List[Tree] = args.zipWithConserve(tpt.tpe.typeParams.map(_.paramVariance)) { (arg, v) =>
              arg.tpe match {
                case _: TypeBounds => transform(arg)
                case _ => atVariance(variance * v)(transform(arg))
              }
            }
            cpy.AppliedTypeTree(tree)(transform(tpt), args1)
        case _ =>
          super.transform(tree)
      }

      private def transformTypeBindingTypeDef(tdef: TypeDef, buff: mutable.Builder[Tree, List[Tree]])(implicit ctx: Context): Tree = {
        if (variance == -1)
          tdef.symbol.addAnnotation(Annotation(New(ref(defn.InternalQuoted_fromAboveAnnot.typeRef)).withSpan(tdef.span)))
        val bindingType = getBinding(tdef.symbol).symbol.typeRef
        val bindingTypeTpe = AppliedType(defn.QuotedTypeClass.typeRef, bindingType :: Nil)
        assert(tdef.name.startsWith("$"))
        val bindName = tdef.name.toString.stripPrefix("$").toTermName
        val sym = ctx0.newPatternBoundSymbol(bindName, bindingTypeTpe, tdef.span, flags = ImplicitTerm)
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
            case tp: TypeRef if tp.typeSymbol == defn.QuotedType_splice =>
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
   *  within the quotes become patterns again and typed acordingly.
   *
   *  ```
   *  case '{ ($ls: List[$t]) } =>
   *    // `t` is of type `Type[T$1]` for some unknown T$1
   *    // `t` is implicitly available
   *    // `l` is of type `Expr[List[T$1]]`
   *    '{ val h: $t = $ls.head  }
   *  ```
   *
   *  For each type splice we will create a new type binding in the pattern match ($t @ _ in this case)
   *  and a corresponding type in the quoted pattern as a hole (@patternBindHole type $t in this case).
   *  All these generated types are inserted at the start of the quoted code.
   *
   *  After typing the tree will resemble
   *
   *  ```
   *  case '{ type ${given t: Type[$t @ _]}; ${ls: Expr[List[$t]]} } => ...
   *  ```
   *
   *  Then the pattern is _split_ into the expression containd in the pattern replacing the splices by holes,
   *  and the patterns in the splices. All these are recombined into a call to `Matcher.unapply`.
   *
   *  ```
   *  case scala.internal.quoted.Expr.unapply[
   *          Tuple1[$t @ _], // Type binging definition
   *          Tuple2[Type[$t], Expr[List[$t]]] // Typing the result of the pattern match
   *        ](
   *          Tuple2.unapply
   *            [Type[$t], Expr[List[$t]]] //Propagated from the tuple above
   *            (implict t @ _, ls @ _: Expr[List[$t]]) // from the spliced patterns
   *        )(
   *         '{ // Runtime quote Matcher.unapply uses to mach against. Expression directly inside the quoted pattern without the splices
   *            @scala.internal.Quoted.patternBindHole type $t
   *            scala.internal.Quoted.patternHole[List[$t]]
   *          },
   *          true, // If there is at least one type splice. Used to instantiate the context with or without GADT constraints
   *          x$2 // tasty.Reflection instance
   *        ) => ...
   *  ```
   */
  private def typedQuotePattern(tree: untpd.Quote, pt: Type)(implicit ctx: Context): Tree = {
    val qctx = inferImplicitArg(defn.QuoteContextClass.typeRef, tree.span)
    if (level == 0 && qctx.tpe.isInstanceOf[SearchFailureType])
      ctx.error(missingArgMsg(qctx, defn.QuoteContextClass.typeRef, ""), ctx.source.atSpan(tree.span))

    val quoted = tree.quoted
    val exprPt = pt.baseType(defn.QuotedExprClass)
    val quotedPt = exprPt.argInfos.headOption match {
      case Some(argPt: ValueType) => argPt // excludes TypeBounds
      case _ => defn.AnyType
    }
    val quoted0 = desugar.quotedPattern(quoted, untpd.TypedSplice(TypeTree(quotedPt)))
    val quoted1 = typedExpr(quoted0, WildcardType)(quoteContext.addMode(Mode.QuotedPattern))

    val (typeBindings, shape, splices) = splitQuotePattern(quoted1)

    class ReplaceBindings extends TypeMap() {
      override def apply(tp: Type): Type = tp match {
        case tp: TypeRef =>
          val tp1 = if (tp.typeSymbol == defn.QuotedType_splice) tp.dealias else tp
          typeBindings.get(tp1.typeSymbol).fold(tp)(_.symbol.typeRef)
        case tp => mapOver(tp)
      }
    }
    val replaceBindings = new ReplaceBindings
    val patType = defn.tupleType(splices.tpes.map(tpe => replaceBindings(tpe.widen)))

    val typeBindingsTuple = tpd.tupleTypeTree(typeBindings.values.toList)

    val replaceBindingsInTree = new TreeMap {
      private[this] var bindMap = Map.empty[Symbol, Symbol]
      override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree =
        tree match {
          case tree: Bind =>
            val sym = tree.symbol
            val newInfo = replaceBindings(sym.info)
            val newSym = ctx.newSymbol(sym.owner, sym.name, sym.flags, newInfo, sym.privateWithin, sym.coord)
            bindMap += sym -> newSym
            Bind(newSym, transform(tree.body)).withSpan(sym.span)
          case _ =>
            super.transform(tree).withType(replaceBindingsInType(tree.tpe))
        }
      private[this] val replaceBindingsInType = new ReplaceBindings {
        override def apply(tp: Type): Type = tp match {
          case tp: TermRef => bindMap.get(tp.termSymbol).fold[Type](tp)(_.typeRef)
          case tp => super.apply(tp)
        }
      }
    }

    val splicePat = typed(untpd.Tuple(splices.map(x => untpd.TypedSplice(replaceBindingsInTree.transform(x)))).withSpan(quoted.span), patType)

    val unapplySym = if (tree.quoted.isTerm) defn.InternalQuotedExpr_unapply else defn.InternalQuotedType_unapply
    val quoteClass = if (tree.quoted.isTerm) defn.QuotedExprClass else defn.QuotedTypeClass
    val quotedPattern =
      if (tree.quoted.isTerm) ref(defn.InternalQuoted_exprQuote.termRef).appliedToType(defn.AnyType).appliedTo(shape).select(nme.apply).appliedTo(qctx)
      else ref(defn.InternalQuoted_typeQuote.termRef).appliedToTypeTrees(shape :: Nil)
    UnApply(
      fun = ref(unapplySym.termRef).appliedToTypeTrees(typeBindingsTuple :: TypeTree(patType) :: Nil),
      implicits = quotedPattern :: Literal(Constant(typeBindings.nonEmpty)) :: qctx :: Nil,
      patterns = splicePat :: Nil,
      proto = quoteClass.typeRef.appliedTo(replaceBindings(quoted1.tpe) & quotedPt))
  }
}

