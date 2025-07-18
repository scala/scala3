package dotty.tools.scaladoc
package tasty

import scala.annotation.*
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.util.control.NonFatal

import NameNormalizer.*
import SyntheticsSupport.*

trait TypesSupport:
  self: TastyParser =>

  type SSignature = List[SignaturePart]

  given TreeSyntax: AnyRef with
    extension (using Quotes)(tpeTree: reflect.Tree)
      def asSignature(elideThis: reflect.ClassDef, originalOwner: reflect.Symbol, skipThisTypePrefix: Boolean): SSignature =
        import reflect._
        tpeTree match
          case TypeBoundsTree(low, high) => typeBoundsTreeOfHigherKindedType(low.tpe, high.tpe, skipThisTypePrefix)(using elideThis, originalOwner)
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

  private def tpe(str: String, dri: DRI): SignaturePart = dotty.tools.scaladoc.Type(str, Some(dri))

  private def tpe(str: String): SignaturePart = dotty.tools.scaladoc.Type(str, None)

  protected def inParens(s: SSignature, wrap: Boolean = true) =
    if wrap then plain("(").l ++ s ++ plain(")").l else s

  extension (on: SignaturePart) def l: List[SignaturePart] = List(on)

  private def tpe(using Quotes)(symbol: reflect.Symbol): SSignature =
    import SymOps._
    val dri: Option[DRI] = Option(symbol).filterNot(_.isHiddenByVisibility).map(_.dri)
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
  @nowarn("id=E219")
  private def inner(
    using Quotes,
  )(
    tp: reflect.TypeRepr,
    skipThisTypePrefix: Boolean
  )(using
    elideThis: reflect.ClassDef,
    originalOwner: reflect.Symbol,
    indent: Int = 0,
    skipTypeSuffix: Boolean = false,
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
      case ByNameType(tpe) => keyword("=> ") :: inner(tpe, skipThisTypePrefix)
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
      case AnnotatedType(tpe, _) =>
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
          tpe(normalizedName).l ++ inner(typ, skipThisTypePrefix)
        }) ++ plain("]").l
        ++ keyword(" =>> ").l
        ++ inner(resType, skipThisTypePrefix)

      case Refinement(parent, "apply", mt : MethodType) if isPolyOrEreased(parent) =>
        val isCtx = isContextualMethod(mt)
        val sym = defn.FunctionClass(mt.paramTypes.length, isCtx)
        val at = sym.typeRef.appliedTo(mt.paramTypes :+ mt.resType)
        inner(Refinement(at, "apply", mt), skipThisTypePrefix)

      case r: Refinement => { //(parent, name, info)
        def getRefinementInformation(t: TypeRepr): List[TypeRepr] = t match {
          case r: Refinement => getRefinementInformation(r.parent) :+ r
          case t => List(t)
        }

        def getParamBounds(t: PolyType): SSignature = commas(
          t.paramNames.zip(t.paramBounds.map(inner(_, skipThisTypePrefix)))
            .map(b => tpe(b(0)).l ++ b(1))
        )

        def getParamList(m: MethodType): SSignature =
          plain("(").l
          ++ m.paramNames.zip(m.paramTypes).map{ case (name, tp) => plain(s"$name: ").l ++ inner(tp, skipThisTypePrefix)}
            .reduceLeftOption((acc: SSignature, elem: SSignature) => acc ++ plain(", ").l ++ elem).getOrElse(List())
          ++ plain(")").l

        def parseRefinedElem(name: String, info: TypeRepr, polyTyped: SSignature = Nil): SSignature = ( info match {
          case m: MethodType => {
            val paramList = getParamList(m)
            keyword("def ").l ++ plain(name).l ++ polyTyped ++ paramList ++ plain(": ").l ++ inner(m.resType, skipThisTypePrefix)
          }
          case t: PolyType => {
            val paramBounds = getParamBounds(t)
            val parsedMethod = parseRefinedElem(name, t.resType)
            if (!paramBounds.isEmpty){
              parseRefinedElem(name, t.resType, plain("[").l ++ paramBounds ++ plain("]").l)
            } else parseRefinedElem(name, t.resType)
          }
          case ByNameType(tp) => keyword("def ").l ++ plain(s"$name: ").l ++ inner(tp, skipThisTypePrefix)
          case t: TypeBounds => keyword("type ").l ++ plain(name).l ++ inner(t, skipThisTypePrefix)
          case t: TypeRef => keyword("val ").l ++ plain(s"$name: ").l ++ inner(t, skipThisTypePrefix)
          case t: TermRef => keyword("val ").l ++ plain(s"$name: ").l ++ inner(t, skipThisTypePrefix)
          case other => noSupported(s"Not supported type in refinement $info")
        } ) ++ plain("; ").l

        def parsePolyFunction(info: TypeRepr): SSignature = info match {
          case t: PolyType =>
            val paramBounds = getParamBounds(t)
            val method = t.resType.asInstanceOf[MethodType]
            val rest = parseDependentFunctionType(method)
            plain("[").l ++ paramBounds ++ plain("]").l ++ keyword(" => ").l ++ rest
          case other => noSupported(s"Not supported type in refinement $info")
        }

        def parseDependentFunctionType(info: TypeRepr): SSignature = info match {
          case m: MethodType =>
            val isCtx = isContextualMethod(m)
            if isDependentMethod(m) then
              val paramList = getParamList(m)
              val arrow = keyword(if isCtx then " ?=> " else " => ").l
              val resType = inner(m.resType, skipThisTypePrefix)
              paramList ++ arrow ++ resType
            else
              val sym = defn.FunctionClass(m.paramTypes.length, isCtx)
              inner(sym.typeRef.appliedTo(m.paramTypes :+ m.resType), skipThisTypePrefix)
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
        val arrow = if t.isContextFunctionType then " ?=> " else " => "
        args match
          case Nil => Nil
          case List(rtpe) => plain("()").l ++ keyword(arrow).l ++ inner(rtpe, skipThisTypePrefix)
          case List(arg, rtpe) =>
            val wrapInParens = stripAnnotated(arg) match
              case _: TermRef | _: TypeRef | _: ConstantType | _: ParamRef => false
              case at: AppliedType if !isInfix(at) && !at.isFunctionType && !at.isTupleN => false
              case _ => true
            inParens(inner(arg, skipThisTypePrefix), wrapInParens) ++ keyword(arrow).l ++ inner(rtpe, skipThisTypePrefix)
          case _ =>
            plain("(").l ++ commas(args.init.map(inner(_, skipThisTypePrefix))) ++ plain(")").l ++ keyword(arrow).l ++ inner(args.last, skipThisTypePrefix)

      case t @ AppliedType(tpe, typeList) =>
        inner(tpe, skipThisTypePrefix) ++ plain("[").l ++ commas(typeList.map { t => t match
          case _: TypeBounds => keyword("_").l ++ inner(t, skipThisTypePrefix)
          case _ => topLevelProcess(t, skipThisTypePrefix)
        }) ++ plain("]").l

      case tp @ TypeRef(qual, typeName) =>
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
                val sig = inParens(inner(qual, skipThisTypePrefix)(using skipTypeSuffix = true), shouldWrapInParens(qual, tp, true))
                sig ++ plain(".").l ++ tpe(tp.typeSymbol)

          case t if skipPrefix(t, elideThis, originalOwner, skipThisTypePrefix) =>
            tpe(tp.typeSymbol)
          case _: TermRef | _: ParamRef =>
            val suffix = if tp.typeSymbol == Symbol.noSymbol then tpe(typeName).l else tpe(tp.typeSymbol)
            inner(qual, skipThisTypePrefix)(using skipTypeSuffix = true) ++ plain(".").l ++ suffix
          case _ =>
            val sig = inParens(inner(qual, skipThisTypePrefix), shouldWrapInParens(qual, tp, true))
            sig ++ keyword("#").l ++ tpe(tp.typeSymbol)
        }

      case tr @ TermRef(qual, typeName) =>
        val prefix = qual match
          case t if skipPrefix(t, elideThis, originalOwner, skipThisTypePrefix) => Nil
          case tp => inner(tp, skipThisTypePrefix)(using skipTypeSuffix = true) ++ plain(".").l
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
            keyword(caseSpaces + "case ").l ++ inner(from, skipThisTypePrefix) ++ keyword(" => ").l ++ inner(to, skipThisTypePrefix)(using indent = indent + 2) ++ plain("\n").l
          case TypeLambda(_, _, MatchCase(from, to)) =>
            keyword(caseSpaces + "case ").l ++ inner(from, skipThisTypePrefix) ++ keyword(" => ").l ++ inner(to, skipThisTypePrefix)(using indent = indent + 2) ++ plain("\n").l
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

  private def typeBound(using Quotes)(t: reflect.TypeRepr, low: Boolean, skipThisTypePrefix: Boolean)(using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol) =
    import reflect._
    val ignore = if (low) t.typeSymbol == defn.NothingClass else t.typeSymbol == defn.AnyClass
    val prefix = keyword(if low then " >: " else " <: ")
    t match {
      case l: TypeLambda => prefix :: inParens(inner(l, skipThisTypePrefix)(using elideThis, originalOwner))
      case p: ParamRef => prefix :: inner(p, skipThisTypePrefix)(using elideThis, originalOwner)
      case other if !ignore => prefix :: topLevelProcess(other, skipThisTypePrefix)(using elideThis, originalOwner)
      case _ => Nil
    }

  private def typeBoundsTreeOfHigherKindedType(using Quotes)(low: reflect.TypeRepr, high: reflect.TypeRepr, skipThisTypePrefix: Boolean)(
    using elideThis: reflect.ClassDef, originalOwner: reflect.Symbol
  ) =
    import reflect._
    def regularTypeBounds(low: TypeRepr, high: TypeRepr) =
      if low == high then keyword(" = ").l ++ inner(low, skipThisTypePrefix)(using elideThis, originalOwner)
      else typeBound(low, low = true, skipThisTypePrefix)(using elideThis, originalOwner) ++ typeBound(high, low = false, skipThisTypePrefix)(using elideThis, originalOwner)
    high.match
      case TypeLambda(params, paramBounds, resType) =>
        if resType.typeSymbol == defn.AnyClass then
          plain("[").l ++ commas(params.zip(paramBounds).map { (name, typ) =>
            val normalizedName = if name.matches("_\\$\\d*") then "_" else name
            tpe(normalizedName).l ++ inner(typ, skipThisTypePrefix)(using elideThis, originalOwner)
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
