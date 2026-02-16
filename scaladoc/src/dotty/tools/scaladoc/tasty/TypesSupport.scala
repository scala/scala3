package dotty.tools.scaladoc
package tasty

import scala.annotation.*
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.util.control.NonFatal

import dotty.tools.scaladoc.cc.*

import NameNormalizer._
import SyntheticsSupport._

trait TypesSupport:
  self: TastyParser =>

  type SSignature = List[SignaturePart]

  given TreeSyntax: AnyRef with
    extension (using Quotes)(tpeTree: reflect.Tree)
      def asSignature(elideThis: reflect.ClassDef, originalOwner: reflect.Symbol, skipThisTypePrefix: Boolean): SSignature =
        import reflect._
        tpeTree match
          case TypeBoundsTree(low, high) => typeBoundsTreeOfHigherKindedType(low.tpe, high.tpe, skipThisTypePrefix)(using elideThis, originalOwner, inCC = None)
          case tpeTree: TypeTree => topLevelProcess(tpeTree.tpe, skipThisTypePrefix)(using elideThis, originalOwner)
          case term: Term => topLevelProcess(term.tpe, skipThisTypePrefix)(using elideThis, originalOwner)
      def asSignature(elideThis: reflect.ClassDef, originalOwner: reflect.Symbol): SSignature =
        tpeTree.asSignature(elideThis, originalOwner, skipThisTypePrefix = false)

  given TypeSyntax: AnyRef with
    extension (using Quotes)(tpe: reflect.TypeRepr)
      def asSignature(elideThis: reflect.ClassDef, originalOwner: reflect.Symbol, skipThisTypePrefix: Boolean): SSignature =
        topLevelProcess(tpe, skipThisTypePrefix)(using elideThis, originalOwner)
      def asSignature(elideThis: reflect.ClassDef, originalOwner: reflect.Symbol): SSignature =
        tpe.asSignature(elideThis, originalOwner, skipThisTypePrefix = false)


  private def plain(str: String): SignaturePart = Plain(str)

  private def keyword(str: String): SignaturePart = Keyword(str)

  private def tpe(str: String, dri: DRI)(using inCC: Option[Any]): SignaturePart =
    if ccEnabled && inCC.isDefined then
      dotty.tools.scaladoc.Plain(str)
    else
      dotty.tools.scaladoc.Type(str, Some(dri))

  private def tpe(str: String)(using inCC: Option[Any]): SignaturePart =
    if ccEnabled && inCC.isDefined then
      dotty.tools.scaladoc.Plain(str)
    else
      dotty.tools.scaladoc.Type(str, None)

  protected def inParens(s: SSignature, wrap: Boolean = true) =
    if wrap then plain("(").l ++ s ++ plain(")").l else s

  extension (on: SignaturePart) def l: List[SignaturePart] = List(on)

  private def tpe(using Quotes)(symbol: reflect.Symbol)(using inCC: Option[Any]): SSignature =
    import SymOps._
    val dri: Option[DRI] = Option(symbol).filterNot(_.isHiddenByVisibility).map(_.dri)
    if ccEnabled && inCC.isDefined then // we are in the context of a capture set and want paths to be rendered plainly
      dotty.tools.scaladoc.Plain(symbol.normalizedName).l
    else
      dotty.tools.scaladoc.Type(symbol.normalizedName, dri).l

  private def commas(lists: List[SSignature]) = lists match
    case List(single) => single
    case other => other.reduce((r, e) => r ++ plain(", ").l ++ e)

  private def isRepeatedAnnotation(using Quotes)(term: reflect.Term) =
    import reflect._
    term.tpe match
      case t: TypeRef => t.name == "Repeated" && t.qualifier.match
        case ThisType(tref: TypeRef) if tref.name == "internal" => true
        case _ => false
      case _ => false

  private def isRepeated(using Quotes)(typeRepr: reflect.TypeRepr) =
    import reflect._
    typeRepr match
      case t: TypeRef => t.name == "<repeated>" && t.qualifier.match
        case ThisType(tref: TypeRef) if tref.name == "scala" => true
        case _ => false
      case _ => false

  private def topLevelProcess(using Quotes)(tp: reflect.TypeRepr, skipThisTypePrefix: Boolean)(using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol): SSignature =
    import reflect._
    tp match
      case ThisType(tpe) =>
        val suffix = List(keyword("this"), plain("."), keyword("type"))
        if skipPrefix(tp, elideThis, originalOwner, skipThisTypePrefix) then suffix
        else inner(tpe, skipThisTypePrefix) ++ plain(".").l ++ suffix
      case tpe => inner(tpe, skipThisTypePrefix)

  // TODO #23 add support for all types signatures that make sense
  private def inner(
    using qctx: Quotes,
  )(
    tp: reflect.TypeRepr,
    skipThisTypePrefix: Boolean
  )(using
    elideThis: reflect.ClassDef,
    originalOwner: reflect.Symbol,
    indent: Int = 0,
    skipTypeSuffix: Boolean = false,
    // inCC means in capture-checking context. If defined, it carries the current capture-set contents.
    inCC: Option[List[reflect.TypeRepr]] = None,
  ): SSignature =
    import reflect._
    def noSupported(name: String): SSignature =
      report.warning(s"Unsupported type: $name: ${tp.show}")
      plain(s"Unsupported[$name]").l
    tp match
      case OrType(left, right) =>
        inParens(inner(left, skipThisTypePrefix), shouldWrapInParens(left, tp, true))
        ++ keyword(" | ").l
        ++ inParens(inner(right, skipThisTypePrefix), shouldWrapInParens(right, tp, false))
      case AndType(left, right) =>
        inParens(inner(left, skipThisTypePrefix), shouldWrapInParens(left, tp, true))
        ++ keyword(" & ").l
        ++ inParens(inner(right, skipThisTypePrefix), shouldWrapInParens(right, tp, false))
      case ByNameType(CapturingType(tpe, refs)) =>
        emitByNameArrow(using qctx)(Some(refs), skipThisTypePrefix) ++ (plain(" ") :: inner(tpe, skipThisTypePrefix))
      case ByNameType(tpe) =>
        emitByNameArrow(using qctx)(None, skipThisTypePrefix) ++ (plain(" ") :: inner(tpe, skipThisTypePrefix))
      case ConstantType(constant) =>
        plain(constant.show).l
      case ThisType(tpe) =>
        val prefix = if skipPrefix(tp, elideThis, originalOwner, skipThisTypePrefix) then Nil else inner(tpe, skipThisTypePrefix) ++ plain(".").l
        val suffix = if skipTypeSuffix then Nil else List(plain("."), keyword("type"))
        prefix ++ keyword("this").l ++ suffix
      case AnnotatedType(AppliedType(_, Seq(tpe)), annotation) if isRepeatedAnnotation(annotation) =>
        inner(tpe, skipThisTypePrefix) :+ plain("*")
      case AppliedType(repeatedClass, Seq(tpe)) if isRepeated(repeatedClass) =>
        inner(tpe, skipThisTypePrefix) :+ plain("*")
      case CapturingType(base, refs) if ccEnabled =>
        base match
          case t @ AppliedType(base, args) if t.isFunctionType =>
            functionType(base, args, skipThisTypePrefix)(using inCC = Some(refs))
          case t : Refinement if t.isFunctionType =>
            inner(base, skipThisTypePrefix)(using indent = indent, skipTypeSuffix = skipTypeSuffix, inCC = Some(refs))
          case t if t.isCapSet => emitCaptureSet(refs, skipThisTypePrefix, omitCap = false)
          case t if t.isPureClass(elideThis) => inner(base, skipThisTypePrefix)
          case t => inner(base, skipThisTypePrefix) ++ emitCapturing(refs, skipThisTypePrefix)
      case AnnotatedType(tpe, _) =>
        inner(tpe, skipThisTypePrefix)
      case FlexibleType(tpe) =>
        inner(tpe, skipThisTypePrefix)
      case tl @ TypeLambda(params, paramBounds, AppliedType(tpe, args))
        if paramBounds.forall { case TypeBounds(low, hi) => low.typeSymbol == defn.NothingClass && hi.typeSymbol == defn.AnyClass }
        && params.length == args.length
        && args.zipWithIndex.forall(_ == tl.param(_)) =>
          // simplify type lambdas such as [X, Y] =>> Map[X, Y] to just Map
          inner(tpe, skipThisTypePrefix)
      case tl @ TypeLambda(params, paramBounds, resType) =>
        plain("[").l ++ commas(params.zip(paramBounds).map { (name, typ) =>
          val normalizedName = if name.matches("_\\$\\d*") then "_" else name
          val suffix = if ccEnabled && typ.derivesFrom(CaptureDefs.Caps_CapSet) then List(Keyword("^")) else Nil
          tpe(normalizedName).l ++ suffix ++ inner(typ, skipThisTypePrefix)
        }) ++ plain("]").l
        ++ keyword(" =>> ").l
        ++ inner(resType, skipThisTypePrefix)

      case Refinement(parent, "apply", mt : MethodType) if isPolyOrEreased(parent) =>
        val isCtx = isContextualMethod(mt)
        val sym = defn.FunctionClass(mt.paramTypes.length, isCtx)
        val at = sym.typeRef.appliedTo(mt.paramTypes :+ mt.resType)
        inner(Refinement(at, "apply", mt), skipThisTypePrefix)

      case r: Refinement => { //(parent, name, info)
        val inCC0 = inCC
        given Option[List[TypeRepr]] = None // do not propagate capture set beyond this point
        def getRefinementInformation(t: TypeRepr): List[TypeRepr] = t match {
          case r: Refinement => getRefinementInformation(r.parent) :+ r
          case t => List(t)
        }

        def getParamBounds(t: PolyType): SSignature = commas(
          t.paramNames.zip(t.paramBounds.map(inner(_, skipThisTypePrefix))).zipWithIndex
            .map { case ((name, bound), idx) =>
              val suffix = if ccEnabled && t.param(idx).derivesFrom(CaptureDefs.Caps_CapSet) then List(Keyword("^")) else Nil
              tpe(name).l ++ suffix ++ bound
            }
        )

        def getParamList(m: MethodType): SSignature =
          plain("(").l
          ++ m.paramNames.zip(m.paramTypes).map{ case (name, tp) => plain(s"$name: ").l ++ inner(tp, skipThisTypePrefix)}
            .reduceLeftOption((acc: SSignature, elem: SSignature) => acc ++ plain(", ").l ++ elem).getOrElse(List())
          ++ plain(")").l

        def parseRefinedElem(name: String, info: TypeRepr, polyTyped: SSignature = Nil): SSignature =
          val ssig = info match
          case m: MethodType => {
            val paramList = getParamList(m)
            keyword("def ").l ++ plain(name).l ++ polyTyped ++ paramList ++ plain(": ").l ++ inner(m.resType, skipThisTypePrefix)
          }
          case t: PolyType =>
            val paramBounds = getParamBounds(t)
            if !paramBounds.isEmpty then
              parseRefinedElem(name, t.resType, plain("[").l ++ paramBounds ++ plain("]").l)
            else
              parseRefinedElem(name, t.resType, polyTyped = Nil)
          case ByNameType(tp) => keyword("def ").l ++ plain(s"$name: ").l ++ inner(tp, skipThisTypePrefix)
          case t: TypeBounds => keyword("type ").l ++ plain(name).l ++ inner(t, skipThisTypePrefix)
          case t: TypeRef => keyword("val ").l ++ plain(s"$name: ").l ++ inner(t, skipThisTypePrefix)
          case t: TermRef => keyword("val ").l ++ plain(s"$name: ").l ++ inner(t, skipThisTypePrefix)
          case other => noSupported(s"Not supported type in refinement $info")

          ssig ++ plain("; ").l

        def parsePolyFunction(info: TypeRepr): SSignature = info match {
          case t: PolyType =>
            val paramBounds = getParamBounds(t)
            val method = t.resType.asInstanceOf[MethodType]
            val rest = parseDependentFunctionType(method)
            plain("[").l ++ paramBounds ++ plain("]").l ++ keyword(" => ").l ++ rest
          case other => noSupported(s"Not supported type in refinement $info")
        }

        // Check whether a type contains `fresh` anywhere in its structure.
        // This is used to force dependent rendering for function types like
        // `(x: AnyRef^) -> AnyRef^{fresh}` where the result is not syntactically
        // dependent on params but the `fresh` existential is semantically scoped
        // by the function type (see scoped-capabilities.md). We recurse through
        // CapturingType (to look past capture annotations) and AppliedType (to find
        // fresh inside type arguments, e.g. `() -> AnyRef^{fresh}` stored as
        // `Function0[AnyRef^{fresh}]`).
        def resultHasFresh(tp: TypeRepr): Boolean = tp match
          case CapturingType(parent, refs) => refs.exists(_.isFreshCap) || resultHasFresh(parent)
          case AppliedType(_, args) => args.exists(resultHasFresh)
          case _ => false

        def parseDependentFunctionType(info: TypeRepr): SSignature = info match {
          case m: MethodType =>
            val isCtx = isContextualMethod(m)
            // Use dependent rendering (preserving named params and precise arrow) when either:
            // 1. The method is syntactically dependent (result references a param), or
            // 2. CC is enabled and the result contains `fresh`, because `fresh` in a
            //    function result is existentially bound by the function type, making the
            //    dependent form semantically significant (see scoped-capabilities.md).
            if isDependentMethod(m) || (ccEnabled && resultHasFresh(m.resType)) then
              val paramList = getParamList(m)
              val arrPrefix = if isCtx then "?" else ""
              val arrow =
                if ccEnabled then
                  inCC0 match
                    case None | Some(Nil) => keyword(arrPrefix + "->").l
                    case Some(List(c)) if c.isCaptureRoot => keyword(arrPrefix + "=>").l
                    case Some(refs) => keyword(arrPrefix + "->") :: emitCaptureSet(refs, skipThisTypePrefix)
                else keyword(arrPrefix + "=>").l
              val resType = inner(m.resType, skipThisTypePrefix)
              paramList ++ (plain(" ") :: arrow) ++ (plain(" ") :: resType)
            else
              val sym = defn.FunctionClass(m.paramTypes.length, isCtx)
              val inCC = inCC0 match
                case None if ccEnabled =>
                  // For CC, we assume an impure function and hence force the capture set to `^`.
                  // Otherwise, the function will be rendered as pure. We hit this case here when
                  // dealing with polymorphic function types, e.g., the A => Int part of [A] => A => Int.
                  Some(List(CaptureDefs.captureRoot.termRef))
                case other => other
              inner(sym.typeRef.appliedTo(m.paramTypes :+ m.resType), skipThisTypePrefix)(using indent = indent, skipTypeSuffix = skipTypeSuffix, inCC = inCC)
          case other => noSupported("Dependent function type without MethodType refinement")
        }

        val refinementInfo = getRefinementInformation(r)
        val refinedType = refinementInfo.head
        val refinedElems = refinementInfo.tail.collect{ case r: Refinement => r }.toList
        val prefix = if refinedType.typeSymbol != defn.ObjectClass then inner(refinedType, skipThisTypePrefix) ++ plain(" ").l else Nil
        if (refinedType.typeSymbol.fullName == "scala.PolyFunction" && refinedElems.size == 1) {
          parsePolyFunction(refinedElems.head.info)
        }
        else if (r.isDependentFunctionType) {
          parseDependentFunctionType(r.info)
        }
        else {
          prefix ++ plain("{ ").l ++ refinedElems.flatMap(e => parseRefinedElem(e.name, e.info)) ++ plain(" }").l
        }
      }

      case AppliedType(tpe, args) if defn.isTupleClass(tpe.typeSymbol) && args.length > 1 =>
        inParens(commas(args.map(inner(_, skipThisTypePrefix))))

      case AppliedType(namedTuple, List(AppliedType(tuple1, names), AppliedType(tuple2, types)))
          if namedTuple.typeSymbol == Symbol.requiredModule("scala.NamedTuple").typeMember("NamedTuple")
          && defn.isTupleClass(tuple1.typeSymbol) && defn.isTupleClass(tuple2.typeSymbol) && names.length == types.length
          && names.forall { case ConstantType(StringConstant(_)) => true case _ => false } =>
        val elems = names
          .collect { case ConstantType(StringConstant(s)) => s }
          .zip(types)
          .map((name, tpe) => plain(name) +: plain(": ") +: inner(tpe, skipThisTypePrefix))
        inParens(commas(elems))

      case t @ AppliedType(tpe, List(lhs, rhs)) if isInfix(t) =>
        inParens(inner(lhs, skipThisTypePrefix), shouldWrapInParens(lhs, t, true))
        ++ plain(" ").l
        ++ inner(tpe, skipThisTypePrefix)
        ++ plain(" ").l
        ++ inParens(inner(rhs, skipThisTypePrefix), shouldWrapInParens(rhs, t, false))

      case t @ AppliedType(tpe, args) if t.isFunctionType =>
        lazy val dealiased = t.dealiasKeepOpaques
        if tpe.isAnyFunctionType || t == dealiased then
          functionType(tpe, args, skipThisTypePrefix)
        else // i23456
          val AppliedType(tpe, args) = dealiased.asInstanceOf[AppliedType]
          functionType(tpe, args, skipThisTypePrefix)

      case t @ AppliedType(tpe, typeList) =>
        inner(tpe, skipThisTypePrefix) ++ plain("[").l ++ commas(typeList.map { t => t match
          case _: TypeBounds => keyword("_").l ++ inner(t, skipThisTypePrefix)
          case _ => topLevelProcess(t, skipThisTypePrefix)
        }) ++ plain("]").l

      case t : TypeRef if ccEnabled && t.isCapSet => emitCaptureSet(Nil, skipThisTypePrefix)

      case tp @ TypeRef(qual, typeName) =>
        inline def wrapping = shouldWrapInParens(inner = qual, outer = tp, isLeft = true)
        qual match {
          case r: RecursiveThis => tpe(s"this.$typeName").l
          case ThisType(tr) =>
            val typeFromSupertypeConstructor = findSupertype(elideThis, tr.typeSymbol) match
              case Some((sym, AppliedType(tr2, args))) =>
                sym.tree.asInstanceOf[ClassDef].constructor.paramss.headOption match
                  case Some(TypeParamClause(tpc)) =>
                    tpc.zip(args).collectFirst {
                      case (TypeDef(name, _), arg) if name == typeName => arg
                    }.map(inner(_, skipThisTypePrefix))
                  case _ => None
              case _ => None
            typeFromSupertypeConstructor.getOrElse:
              if skipPrefix(qual, elideThis, originalOwner, skipThisTypePrefix) then
                tpe(tp.typeSymbol)
              else
                val sig = inParens(
                  inner(qual, skipThisTypePrefix)(using indent = indent, skipTypeSuffix = true, inCC = inCC), wrapping)
                   sig
                ++ plain(".").l
                ++ tpe(tp.typeSymbol)

          case t if skipPrefix(t, elideThis, originalOwner, skipThisTypePrefix) =>
            tpe(tp.typeSymbol)
          case _: TermRef | _: ParamRef =>
            val suffix = if tp.typeSymbol == Symbol.noSymbol then tpe(typeName).l else tpe(tp.typeSymbol)
               inner(qual, skipThisTypePrefix)(using indent = indent, skipTypeSuffix = true, inCC = inCC)
            ++ plain(".").l
            ++ suffix
          case _ =>
            val sig = inParens(inner(qual, skipThisTypePrefix), wrapping)
            sig ++ keyword("#").l ++ tpe(tp.typeSymbol)
        }

      case tr @ TermRef(qual, typeName) =>
        val prefix = qual match
          case t if skipPrefix(t, elideThis, originalOwner, skipThisTypePrefix) => Nil
          case tp => inner(tp, skipThisTypePrefix)(using indent = indent, skipTypeSuffix = true, inCC = inCC) ++ plain(".").l
        val suffix = if skipTypeSuffix then Nil else List(plain("."), keyword("type"))
        val typeSig = tr.termSymbol.tree match
          case vd: ValDef if tr.termSymbol.flags.is(Flags.Module) =>
            inner(vd.tpt.tpe, skipThisTypePrefix)
          case _ => plain(typeName).l
        prefix ++ typeSig ++ suffix

      case TypeBounds(low, hi) =>
        if(low == hi) keyword(" = ").l ++ inner(low, skipThisTypePrefix)
        else typeBoundsTreeOfHigherKindedType(low, hi, skipThisTypePrefix)

      case NoPrefix() => Nil

      case MatchType(bond, sc, cases) =>
        val caseSpaces = " " * (indent + 2)
        val spaces = " " * (indent)
        val casesTexts = cases.flatMap {
          case MatchCase(from, to) =>
               keyword(caseSpaces + "case ").l
            ++ inner(from, skipThisTypePrefix)
            ++ keyword(" => ").l
            ++ inner(to, skipThisTypePrefix)(using indent = indent + 2, skipTypeSuffix = skipTypeSuffix, inCC = inCC)
            ++ plain("\n").l
          case TypeLambda(_, _, MatchCase(from, to)) =>
               keyword(caseSpaces + "case ").l
            ++ inner(from, skipThisTypePrefix)
            ++ keyword(" => ").l
            ++ inner(to, skipThisTypePrefix)(using indent = indent + 2, skipTypeSuffix = skipTypeSuffix, inCC = inCC)
            ++ plain("\n").l
        }
        inner(sc, skipThisTypePrefix) ++ keyword(" match ").l ++ plain("{\n").l ++ casesTexts ++ plain(spaces + "}").l

      case ParamRef(m: MethodType, i) =>
        val suffix = if skipTypeSuffix then Nil else List(plain("."), keyword("type"))
        tpe(m.paramNames(i)).l ++ suffix

      case ParamRef(binder: LambdaType, i) => tpe(binder.paramNames(i)).l

      case RecursiveType(tp) => inner(tp, skipThisTypePrefix)

      case MatchCase(pattern, rhs) =>
        keyword("case ").l ++ inner(pattern, skipThisTypePrefix) ++ keyword(" => ").l ++ inner(rhs, skipThisTypePrefix)

      case t: dotty.tools.dotc.core.Types.LazyRef => try {
        inner(t.ref(using ctx.compilerContext).asInstanceOf[TypeRepr], skipThisTypePrefix)
      } catch {
        case e: AssertionError => tpe("LazyRef(...)").l
      }

      case tpe =>
        val msg = s"Encountered unsupported type. Report this problem to https://github.com/scala/scala3/.\n" +
          s"${tpe.show(using Printer.TypeReprStructure)}"
        throw MatchError(msg)

  private def functionType(using qctx: Quotes)(funTy: reflect.TypeRepr, args: List[reflect.TypeRepr], skipThisTypePrefix: Boolean)(using
    elideThis: reflect.ClassDef,
    originalOwner: reflect.Symbol,
    indent: Int,
    skipTypeSuffix: Boolean,
    inCC: Option[List[reflect.TypeRepr]],
  ): SSignature =
    import reflect._
    val arrow = plain(" ") :: (emitFunctionArrow(using qctx)(funTy, inCC, skipThisTypePrefix) ++ plain(" ").l)
    given Option[List[TypeRepr]] = None // do not propagate capture set beyond this point
    args match
      case Nil => Nil
      case List(rtpe) => plain("()").l ++ arrow ++ inner(rtpe, skipThisTypePrefix)
      case List(arg, rtpe) =>
        val wrapInParens = stripAnnotated(arg) match
          case _: TermRef | _: TypeRef | _: ConstantType | _: ParamRef => false
          case at: AppliedType if !isInfix(at) && !at.isFunctionType && !at.isTupleN => false
          case _ => true
        inParens(inner(arg, skipThisTypePrefix), wrapInParens) ++ arrow ++ inner(rtpe, skipThisTypePrefix)
      case _ =>
        plain("(").l ++ commas(args.init.map(inner(_, skipThisTypePrefix))) ++ plain(")").l ++ arrow ++ inner(args.last, skipThisTypePrefix)

  private def typeBound(using Quotes)(t: reflect.TypeRepr, low: Boolean, skipThisTypePrefix: Boolean)(using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol) =
    import reflect._
    val ignore = low &&  (ccEnabled && t.isCapSetPure
                          || t.typeSymbol == defn.NothingClass)
              || !low && (ccEnabled && t.isCapSetCap
                          || t.typeSymbol == defn.AnyClass)
    val prefix = keyword(if low then " >: " else " <: ")
    t match {
      case l: TypeLambda => prefix :: inParens(inner(l, skipThisTypePrefix)(using elideThis, originalOwner))
      case p: ParamRef => prefix :: inner(p, skipThisTypePrefix)(using elideThis, originalOwner)
      case other if !ignore => prefix :: topLevelProcess(other, skipThisTypePrefix)(using elideThis, originalOwner)
      case _ => Nil
    }

  private def typeBoundsTreeOfHigherKindedType(using Quotes)(low: reflect.TypeRepr, high: reflect.TypeRepr, skipThisTypePrefix: Boolean)(
    using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol, inCC: Option[List[reflect.TypeRepr]]
  ) =
    import reflect._
    def regularTypeBounds(low: TypeRepr, high: TypeRepr) =
      if low == high then keyword(" = ").l ++ inner(low, skipThisTypePrefix)(using elideThis, originalOwner, inCC = inCC)
      else typeBound(low, low = true, skipThisTypePrefix)(using elideThis, originalOwner) ++ typeBound(high, low = false, skipThisTypePrefix)(using elideThis, originalOwner)
    high.match
      case TypeLambda(params, paramBounds, resType) =>
        if resType.typeSymbol == defn.AnyClass then
          plain("[").l ++ commas(params.zip(paramBounds).map { (name, typ) =>
            val normalizedName = if name.matches("_\\$\\d*") then "_" else name
            tpe(normalizedName)(using inCC).l ++ inner(typ, skipThisTypePrefix)(using elideThis, originalOwner, inCC = inCC)
          }) ++ plain("]").l
        else
          regularTypeBounds(low, high)
      case _ => regularTypeBounds(low, high)

  private def findSupertype(using Quotes)(c: reflect.ClassDef, sym: reflect.Symbol) =
    getSupertypes(c).find((s, t) => s == sym)

  private def skipPrefix(using Quotes)(tr: reflect.TypeRepr, elideThis: reflect.ClassDef, originalOwner: reflect.Symbol, skipThisTypePrefix: Boolean) =
    import reflect._

    def findClassOwner(s: Symbol): Symbol =
      if s.isClassDef then s
      else if s.exists then findClassOwner(s.owner)
      else Symbol.noSymbol

    val classOwner = findClassOwner(originalOwner)

    tr match
      case NoPrefix() => true
      case ThisType(tp) if tp.typeSymbol == classOwner || tp.typeSymbol == elideThis.symbol => true
      case ThisType(_) if skipThisTypePrefix => true
      case _ =>
        val flags = tr.typeSymbol.flags
        flags.is(Flags.Module) || flags.is(Flags.Package)

  private def shouldWrapInParens(using Quotes)(inner: reflect.TypeRepr, outer: reflect.TypeRepr, isLeft: Boolean) =
    import reflect._

    (inner, outer) match
      case (_: AndType, _: TypeRef) => true
      case (_: OrType,  _: TypeRef) => true
      case (t: AppliedType, _: TypeRef) => isInfix(t)

      case (_: AndType, _: AndType) => false
      case (_: AndType, _: OrType)  => false
      case (_: OrType,  _: AndType) => true
      case (_: OrType,  _: OrType)  => false

      case (at: AppliedType, _: AndType) => at.isFunctionType || isInfix(at)
      case (at: AppliedType, _: OrType)  => at.isFunctionType || isInfix(at)
      case (_: AndType, at: AppliedType) => isInfix(at)
      case (_: OrType, at: AppliedType)  => isInfix(at)
      case (at1: AppliedType, at2: AppliedType) =>
        val leftAssoc = !at1.tycon.typeSymbol.name.endsWith(":")
        isInfix(at2) && (at1.isFunctionType || isInfix(at1) && (
          at1.tycon.typeSymbol != at2.tycon.typeSymbol || leftAssoc != isLeft
        ))
      case _ => false

  private def isInfix(using Quotes)(at: reflect.AppliedType) =
    import dotty.tools.dotc.util.Chars.isIdentifierPart
    import reflect._

    def infixAnnot =
      at.tycon.typeSymbol.getAnnotation(Symbol.requiredClass("scala.annotation.showAsInfix")) match
        case Some(Apply(_, args)) =>
          args.collectFirst {
            case Literal(BooleanConstant(false)) => false
          }.getOrElse(true)
        case _ => false

    at.args.size == 2 && (!at.typeSymbol.name.forall(isIdentifierPart) || infixAnnot)

  private def isPolyOrEreased(using Quotes)(tr: reflect.TypeRepr) =
    Set("scala.PolyFunction", "scala.runtime.ErasedFunction")
      .contains(tr.typeSymbol.fullName)

  private def isContextualMethod(using Quotes)(mt: reflect.MethodType) =
    mt.asInstanceOf[dotty.tools.dotc.core.Types.MethodType].isContextualMethod

  private def isDependentMethod(using Quotes)(mt: reflect.MethodType) =
    val method = mt.asInstanceOf[dotty.tools.dotc.core.Types.MethodType]
    try method.isParamDependent || method.isResultDependent
    catch case NonFatal(_) => true

  private def stripAnnotated(using Quotes)(tr: reflect.TypeRepr): reflect.TypeRepr =
    import reflect.*
    tr match
      case AnnotatedType(tr, _) => stripAnnotated(tr)
      case other => other

  private def emitCapability(using Quotes)(ref: reflect.TypeRepr, skipThisTypePrefix: Boolean)(using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol): SSignature =
    import reflect._
    ref match
      case ReachCapability(c)     => emitCapability(c, skipThisTypePrefix) :+ Keyword("*")
      case ReadOnlyCapability(c)  => emitCapability(c, skipThisTypePrefix) :+ Keyword(".rd")
      case OnlyCapability(c, cls) => emitCapability(c, skipThisTypePrefix) ++ List(Plain("."), Keyword("only"), Plain("[")) ++ inner(cls.typeRef, skipThisTypePrefix) :+ Plain("]")
      case ThisType(_)            => List(Keyword("this"))
      case t                      => inner(t, skipThisTypePrefix)(using skipTypeSuffix = true, inCC = Some(Nil))

  private def emitCaptureSet(using Quotes)(refs: List[reflect.TypeRepr], skipThisTypePrefix: Boolean, omitCap: Boolean = true)(using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol): SSignature =
    import reflect._
    refs match
      case List(ref) if omitCap && ref.isCaptureRoot => Nil
      case refs =>
        val res0 = refs.map(x => emitCapability(x, skipThisTypePrefix))
        val res1 = res0 match
          case Nil => Nil
          case other => other.reduce((r, e) => r ++ (List(Plain(", ")) ++ e))
        Plain("{") :: (res1 ++ List(Plain("}")))

  // Determines whether a capture set reference should be rendered in the current context.
  // Some capabilities (like `this` in a pure class) are elided. We need to handle all
  // capability wrappers (reach `c*`, read-only `c.rd`, classifier `.only[C]`) by
  // recursing into the underlying capability, and always render root capabilities
  // (`cap`/`any`) and `fresh`.
  private def isCapturedInContext(using Quotes)(ref: reflect.TypeRepr)(using elideThis: reflect.ClassDef): Boolean =
    import reflect._
    ref match
      case t if t.isCaptureRoot   => true
      case t if t.isFreshCap      => true
      case ReachCapability(c)     => isCapturedInContext(c)
      case ReadOnlyCapability(c)  => isCapturedInContext(c)
      case OnlyCapability(c, _)   => isCapturedInContext(c)
      case ThisType(tr)           => !elideThis.symbol.typeRef.isPureClass(elideThis)
      case t                      => !t.isPureClass(elideThis)

  private def emitCapturing(using Quotes)(refs: List[reflect.TypeRepr], skipThisTypePrefix: Boolean)(using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol): SSignature =
    import reflect._
    val refs0 = refs.filter(isCapturedInContext)
    if refs0.isEmpty then Nil else Keyword("^") :: emitCaptureSet(refs0, skipThisTypePrefix)

  private def emitFunctionArrow(using Quotes)(funTy: reflect.TypeRepr, captures: Option[List[reflect.TypeRepr]], skipThisTypePrefix: Boolean)(using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol): SSignature =
    import reflect._
    val isContextFun = funTy.isAnyContextFunction || funTy.isAnyImpureContextFunction
    val prefix = if isContextFun then "?" else ""
    if !ccEnabled then
      List(Keyword(prefix + "=>"))
    else
      val isPureFun = funTy.isAnyFunction || funTy.isAnyContextFunction
      val isImpureFun = funTy.isAnyImpureFunction || funTy.isAnyImpureContextFunction
      captures match
        case None => // means an explicit retains* annotation is missing
          if isPureFun then
            List(Keyword(prefix + "->"))
          else if isImpureFun then
            List(Keyword(prefix + "=>"))
          else
            report.error(s"Cannot emit function arrow: expected a (Context)Function* or Impure(Context)Function*, but got: ${funTy.show}")
            Nil
        case Some(refs) =>
          // there is some capture set
          refs match
            case Nil => List(Keyword(prefix + "->"))
            case List(ref) if ref.isCaptureRoot => List(Keyword(prefix + "=>"))
            case refs => Keyword(prefix + "->") :: emitCaptureSet(refs, skipThisTypePrefix)

  private def emitByNameArrow(using Quotes)(captures: Option[List[reflect.TypeRepr]], skipThisTypePrefix: Boolean)(using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol): SSignature =
    emitFunctionArrow(CaptureDefs.Function1.typeRef, captures, skipThisTypePrefix)
