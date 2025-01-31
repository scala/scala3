package dotty.tools.scaladoc
package tasty

import scala.jdk.CollectionConverters._

import scala.quoted._

import NameNormalizer._
import SyntheticsSupport._

trait TypesSupport:
  self: TastyParser =>

  type SSignature = List[SignaturePart]

  given TreeSyntax: AnyRef with
    extension (using Quotes)(tpeTree: reflect.Tree)
      def asSignature(elideThis: reflect.ClassDef): SSignature =
        import reflect._
        tpeTree match
          case TypeBoundsTree(low, high) => typeBoundsTreeOfHigherKindedType(low.tpe, high.tpe)(using elideThis)
          case tpeTree: TypeTree => topLevelProcess(tpeTree.tpe)(using elideThis)
          case term: Term => topLevelProcess(term.tpe)(using elideThis)

  given TypeSyntax: AnyRef with
    extension (using Quotes)(tpe: reflect.TypeRepr)
      def asSignature(elideThis: reflect.ClassDef): SSignature =
        topLevelProcess(tpe)(using elideThis)


  private def plain(str: String): SignaturePart = Plain(str)

  private def keyword(str: String): SignaturePart = Keyword(str)

  private def tpe(str: String, dri: DRI): SignaturePart = dotty.tools.scaladoc.Type(str, Some(dri))

  private def tpe(str: String): SignaturePart = dotty.tools.scaladoc.Type(str, None)

  private def inParens(s: SSignature, wrap: Boolean = true) =
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

  private def topLevelProcess(using Quotes)(tp: reflect.TypeRepr)(using elideThis: reflect.ClassDef): SSignature =
    import reflect._
    tp match
      case ThisType(tpe) =>
        val suffix = List(keyword("this"), plain("."), keyword("type"))
        if skipPrefix(tp, elideThis) then suffix
        else inner(tpe) ++ plain(".").l ++ suffix
      case tpe => inner(tpe)

  // TODO #23 add support for all types signatures that make sense
  private def inner(
    using Quotes,
  )(
    tp: reflect.TypeRepr,
  )(using
    elideThis: reflect.ClassDef,
    indent: Int = 0,
    skipTypeSuffix: Boolean = false,
  ): SSignature =
    import reflect._
    def noSupported(name: String): SSignature =
      report.warning(s"Unsupported type: $name: ${tp.show}")
      plain(s"Unsupported[$name]").l
    tp match
      case OrType(left, right) =>
        inParens(inner(left), shouldWrapInParens(left, tp, true))
        ++ keyword(" | ").l
        ++ inParens(inner(right), shouldWrapInParens(right, tp, false))
      case AndType(left, right) =>
        inParens(inner(left), shouldWrapInParens(left, tp, true))
        ++ keyword(" & ").l
        ++ inParens(inner(right), shouldWrapInParens(right, tp, false))
      case ByNameType(tpe) => keyword("=> ") :: inner(tpe)
      case ConstantType(constant) =>
        plain(constant.show).l
      case ThisType(tpe) =>
        val prefix = findSupertype(elideThis, tpe.typeSymbol) match
          case Some(_) => Nil
          case None    => inner(tpe) ++ plain(".").l
        val suffix = if skipTypeSuffix then Nil else List(plain("."), keyword("type"))
        prefix ++ keyword("this").l ++ suffix
      case AnnotatedType(AppliedType(_, Seq(tpe)), annotation) if isRepeatedAnnotation(annotation) =>
        inner(tpe) :+ plain("*")
      case AppliedType(repeatedClass, Seq(tpe)) if isRepeated(repeatedClass) =>
        inner(tpe) :+ plain("*")
      case AnnotatedType(tpe, _) =>
        inner(tpe)
      case tl @ TypeLambda(params, paramBounds, AppliedType(tpe, args))
        if paramBounds.map(inner).forall(_.isEmpty) && params.zip(args.map(inner).flatten.map(_.name)).forall(_ == _) =>
        inner(tpe)
      case tl @ TypeLambda(params, paramBounds, resType) =>
        plain("[").l ++ commas(params.zip(paramBounds).map { (name, typ) =>
          val normalizedName = if name.matches("_\\$\\d*") then "_" else name
          tpe(normalizedName).l ++ inner(typ)
        }) ++ plain("]").l
        ++ keyword(" =>> ").l
        ++ inner(resType)

      case r: Refinement => { //(parent, name, info)
        def getRefinementInformation(t: TypeRepr): List[TypeRepr] = t match {
          case r: Refinement => getRefinementInformation(r.parent) :+ r
          case t => List(t)
        }

        def getParamBounds(t: PolyType): SSignature = commas(
          t.paramNames.zip(t.paramBounds.map(inner(_)))
            .map(b => tpe(b(0)).l ++ b(1))
        )

        def getParamList(m: MethodType): SSignature =
          plain("(").l
          ++ m.paramNames.zip(m.paramTypes).map{ case (name, tp) => plain(s"$name: ").l ++ inner(tp)}
            .reduceLeftOption((acc: SSignature, elem: SSignature) => acc ++ plain(", ").l ++ elem).getOrElse(List())
          ++ plain(")").l

        def parseRefinedElem(name: String, info: TypeRepr, polyTyped: SSignature = Nil): SSignature = ( info match {
          case m: MethodType => {
            val paramList = getParamList(m)
            keyword("def ").l ++ plain(name).l ++ polyTyped ++ paramList ++ plain(": ").l ++ inner(m.resType)
          }
          case t: PolyType => {
            val paramBounds = getParamBounds(t)
            val parsedMethod = parseRefinedElem(name, t.resType)
            if (!paramBounds.isEmpty){
              parseRefinedElem(name, t.resType, plain("[").l ++ paramBounds ++ plain("]").l)
            } else parseRefinedElem(name, t.resType)
          }
          case ByNameType(tp) => keyword("def ").l ++ plain(s"$name: ").l ++ inner(tp)
          case t: TypeBounds => keyword("type ").l ++ plain(name).l ++ inner(t)
          case t: TypeRef => keyword("val ").l ++ plain(s"$name: ").l ++ inner(t)
          case t: TermRef => keyword("val ").l ++ plain(s"$name: ").l ++ inner(t)
          case other => noSupported(s"Not supported type in refinement $info")
        } ) ++ plain("; ").l

        def parsePolyFunction(info: TypeRepr): SSignature = info match {
          case t: PolyType =>
            val paramBounds = getParamBounds(t)
            val method = t.resType.asInstanceOf[MethodType]
            val paramList = getParamList(method)
            val resType = inner(method.resType)
            plain("[").l ++ paramBounds ++ plain("]").l ++ keyword(" => ").l ++ paramList ++ keyword(" => ").l ++ resType
          case other => noSupported(s"Not supported type in refinement $info")
        }

        def parseDependentFunctionType(info: TypeRepr): SSignature = info match {
          case m: MethodType =>
            val paramList = getParamList(m)
            paramList ++ keyword(" => ").l ++ inner(m.resType)
          case other => noSupported("Dependent function type without MethodType refinement")
        }

        val refinementInfo = getRefinementInformation(r)
        val refinedType = refinementInfo.head
        val refinedElems = refinementInfo.tail.collect{ case r: Refinement => r }.toList
        val prefix = if refinedType.typeSymbol != defn.ObjectClass then inner(refinedType) ++ plain(" ").l else Nil
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
      case t @ AppliedType(tpe, typeList) =>
        import dotty.tools.dotc.util.Chars._
        if defn.isTupleClass(tpe.typeSymbol) && typeList.length != 1 then
          typeList match
            case Nil => Nil
            case args => inParens(commas(args.map(inner(_))))
        else if isInfix(t) then
          val lhs = typeList.head
          val rhs = typeList.last
          inParens(inner(lhs), shouldWrapInParens(lhs, t, true))
          ++ plain(" ").l
          ++ inner(tpe)
          ++ plain(" ").l
          ++ inParens(inner(rhs), shouldWrapInParens(rhs, t, false))
        else if t.isFunctionType then
          val arrow = if t.isContextFunctionType then " ?=> " else " => "
          typeList match
            case Nil =>
              Nil
            case Seq(rtpe) =>
              plain("()").l ++ keyword(arrow).l ++ inner(rtpe)
            case Seq(arg, rtpe) =>
              val partOfSignature = arg match
                case _: TermRef | _: TypeRef | _: ConstantType | _: ParamRef => inner(arg)
                case _ => inParens(inner(arg))
              partOfSignature ++ keyword(arrow).l ++ inner(rtpe)
            case args =>
              plain("(").l ++ commas(args.init.map(inner(_))) ++ plain(")").l ++ keyword(arrow).l ++ inner(args.last)
        else inner(tpe) ++ plain("[").l ++ commas(typeList.map { t => t match
          case _: TypeBounds => keyword("_").l ++ inner(t)
          case _ => topLevelProcess(t)
        }) ++ plain("]").l

      case tp @ TypeRef(qual, typeName) =>
        qual match {
          case r: RecursiveThis => tpe(s"this.$typeName").l
          case t if skipPrefix(t, elideThis) =>
            tpe(tp.typeSymbol)
          case _: TermRef | _: ParamRef =>
            val suffix = if tp.typeSymbol == Symbol.noSymbol then tpe(typeName).l else tpe(tp.typeSymbol)
            inner(qual)(using skipTypeSuffix = true) ++ plain(".").l ++ suffix
          case ThisType(tr) =>
            findSupertype(elideThis, tr.typeSymbol) match
              case Some((sym, AppliedType(tr2, args))) =>
                sym.tree.asInstanceOf[ClassDef].constructor.paramss.headOption match
                  case Some(TypeParamClause(tpc)) =>
                    tpc.zip(args).collectFirst {
                      case (TypeDef(name, _), arg) if name == typeName => arg
                    } match
                      case Some(tr) => inner(tr)
                      case None => tpe(tp.typeSymbol)
                  case _ => tpe(tp.typeSymbol)
              case Some(_) => tpe(tp.typeSymbol)
              case None =>
                val sig = inParens(inner(qual)(using skipTypeSuffix = true), shouldWrapInParens(qual, tp, true))
                sig ++ plain(".").l ++ tpe(tp.typeSymbol)
          case _ =>
            val sig = inParens(inner(qual), shouldWrapInParens(qual, tp, true))
            sig ++ keyword("#").l ++ tpe(tp.typeSymbol)
        }

      case tr @ TermRef(qual, typeName) =>
        val prefix = qual match
          case t if skipPrefix(t, elideThis) => Nil
          case tp => inner(tp)(using skipTypeSuffix = true) ++ plain(".").l
        val suffix = if skipTypeSuffix then Nil else List(plain("."), keyword("type"))
        val typeSig = tr.termSymbol.tree match
          case vd: ValDef if tr.termSymbol.flags.is(Flags.Module) =>
            inner(vd.tpt.tpe)
          case _ => plain(typeName).l
        prefix ++ typeSig ++ suffix

      case TypeBounds(low, hi) =>
        if(low == hi) keyword(" = ").l ++ inner(low)
        else typeBoundsTreeOfHigherKindedType(low, hi)

      case NoPrefix() => Nil

      case MatchType(bond, sc, cases) =>
        val caseSpaces = " " * (indent + 2)
        val spaces = " " * (indent)
        val casesTexts = cases.flatMap {
          case MatchCase(from, to) =>
            keyword(caseSpaces + "case ").l ++ inner(from) ++ keyword(" => ").l ++ inner(to)(using indent = indent + 2) ++ plain("\n").l
          case TypeLambda(_, _, MatchCase(from, to)) =>
            keyword(caseSpaces + "case ").l ++ inner(from) ++ keyword(" => ").l ++ inner(to)(using indent = indent + 2) ++ plain("\n").l
        }
        inner(sc) ++ keyword(" match ").l ++ plain("{\n").l ++ casesTexts ++ plain(spaces + "}").l

      case ParamRef(m: MethodType, i) =>
        val suffix = if skipTypeSuffix then Nil else List(plain("."), keyword("type"))
        tpe(m.paramNames(i)).l ++ suffix

      case ParamRef(binder: LambdaType, i) => tpe(binder.paramNames(i)).l

      case RecursiveType(tp) => inner(tp)

      case MatchCase(pattern, rhs) =>
        keyword("case ").l ++ inner(pattern) ++ keyword(" => ").l ++ inner(rhs)

      case t: dotty.tools.dotc.core.Types.LazyRef => try {
        inner(t.ref(using ctx.compilerContext).asInstanceOf[TypeRepr])
      } catch {
        case e: AssertionError => tpe("LazyRef(...)").l
      }

      case tpe =>
        val msg = s"Encountered unsupported type. Report this problem to https://github.com/lampepfl/dotty/.\n" +
          s"${tpe.show(using Printer.TypeReprStructure)}"
        throw MatchError(msg)

  private def typeBound(using Quotes)(t: reflect.TypeRepr, low: Boolean)(using elideThis: reflect.ClassDef) =
    import reflect._
    val ignore = if (low) t.typeSymbol == defn.NothingClass else t.typeSymbol == defn.AnyClass
    val prefix = keyword(if low then " >: " else " <: ")
    t match {
      case l: TypeLambda => prefix :: inParens(inner(l)(using elideThis))
      case p: ParamRef => prefix :: inner(p)(using elideThis)
      case other if !ignore => prefix :: topLevelProcess(other)(using elideThis)
      case _ => Nil
    }

  private def typeBoundsTreeOfHigherKindedType(using Quotes)(low: reflect.TypeRepr, high: reflect.TypeRepr)(using elideThis: reflect.ClassDef) =
    import reflect._
    def regularTypeBounds(low: TypeRepr, high: TypeRepr) =
      if low == high then keyword(" = ").l ++ inner(low)(using elideThis)
      else typeBound(low, low = true)(using elideThis) ++ typeBound(high, low = false)(using elideThis)
    high.match
      case TypeLambda(params, paramBounds, resType) =>
        if resType.typeSymbol == defn.AnyClass then
          plain("[").l ++ commas(params.zip(paramBounds).map { (name, typ) =>
            val normalizedName = if name.matches("_\\$\\d*") then "_" else name
            tpe(normalizedName).l ++ inner(typ)(using elideThis)
          }) ++ plain("]").l
        else
          regularTypeBounds(low, high)
      case _ => regularTypeBounds(low, high)

  private def findSupertype(using Quotes)(c: reflect.ClassDef, sym: reflect.Symbol) =
    getSupertypes(c).find((s, t) => s == sym)

  private def skipPrefix(using Quotes)(tr: reflect.TypeRepr, elideThis: reflect.ClassDef) =
    import reflect._

    def collectOwners(owners: Set[Symbol], sym: Symbol): Set[Symbol] =
      if sym.flags.is(Flags.Package) then owners
      else collectOwners(owners + sym, sym.owner)
    val owners = collectOwners(Set.empty, elideThis.symbol)

    tr match
      case NoPrefix() => true
      case ThisType(tp) if owners(tp.typeSymbol) => true
      case tp if owners(tp.typeSymbol) => true
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
