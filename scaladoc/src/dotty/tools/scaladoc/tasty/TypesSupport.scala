package dotty.tools.scaladoc
package tasty

import collection.JavaConverters._

import scala.quoted._

import NameNormalizer._
import SyntheticsSupport._

trait TypesSupport:
  self: TastyParser =>

  type SSignature = List[SignaturePart]

  def getGivenInstance(method: qctx.reflect.DefDef): Option[SSignature] =
    import qctx.reflect._
    given qctx.type = qctx

    def extractTypeSymbol(t: Tree): Option[Symbol] = t match
      case tpeTree: TypeTree =>
        inner(tpeTree.tpe)
      case other => None

    def inner(tpe: TypeRepr): Option[Symbol] = tpe match
      case ThisType(tpe) => inner(tpe)
      case AnnotatedType(tpe, _) => inner(tpe)
      case AppliedType(tpe, _) => inner(tpe)
      case tp @ TermRef(qual, typeName) => Some(tp.termSymbol)
      case tp @ TypeRef(qual, typeName) => Some(tp.typeSymbol)

    val typeSymbol = extractTypeSymbol(method.returnTpt)

    typeSymbol.map(_.tree).collect {
      case c: ClassDef => c.getTreeOfFirstParent
      case _ => Some(method.returnTpt)
    }.flatten.map(_.asSignature)

  given TreeSyntax: AnyRef with
    extension (using Quotes)(tpeTree: reflect.Tree)
      def asSignature: SSignature =
        import reflect._
        tpeTree match
          case TypeBoundsTree(low, high) => typeBoundsTreeOfHigherKindedType(low.tpe, high.tpe)
          case tpeTree: TypeTree => inner(tpeTree.tpe)
          case term:  Term => inner(term.tpe)

  given TypeSyntax: AnyRef with
    extension (using Quotes)(tpe: reflect.TypeRepr)
      def asSignature: SSignature = inner(tpe)


  private def plain(str: String): SignaturePart = Plain(str)

  private def keyword(str: String): SignaturePart = Keyword(str)

  private def tpe(str: String, dri: DRI): SignaturePart = dotty.tools.scaladoc.Type(str, Some(dri))

  private def tpe(str: String): SignaturePart = dotty.tools.scaladoc.Type(str, None)

  extension (on: SignaturePart) def l: List[SignaturePart] = List(on)

  private def tpe(using Quotes)(symbol: reflect.Symbol): SSignature =
    import SymOps._
    val suffix = if symbol.isValDef || symbol.flags.is(reflect.Flags.Module) then plain(".type").l else Nil
    val dri: Option[DRI] = Option(symbol).filterNot(_.isHiddenByVisibility).map(_.dri)
    dotty.tools.scaladoc.Type(symbol.normalizedName, dri) :: suffix

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

  // TODO #23 add support for all types signatures that makes sense
  private def inner(using Quotes)(tp: reflect.TypeRepr)(using indent: Int = 0): SSignature =
    import reflect._
    def noSupported(name: String): SSignature =
      println(s"WARN: Unsupported type: $name: ${tp.show}")
      plain(s"Unsupported[$name]").l

    tp match
      case OrType(left, right) => inner(left) ++ keyword(" | ").l ++ inner(right)
      case AndType(left, right) => inner(left) ++ keyword(" & ").l ++ inner(right)
      case ByNameType(tpe) => keyword("=> ") :: inner(tpe)
      case ConstantType(constant) =>
        plain(constant.show).l
      case ThisType(tpe) => inner(tpe)
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
        val refinementInfo = getRefinementInformation(r)
        val refinedType = refinementInfo.head
        val refinedElems = refinementInfo.tail.collect{ case r: Refinement => r }.toList
        val prefix = if refinedType.typeSymbol != defn.ObjectClass then inner(refinedType) ++ plain(" ").l else Nil
        if (refinedType.typeSymbol.fullName == "scala.PolyFunction" && refinedElems.size == 1) {
          parsePolyFunction(refinedElems.head.info)
        } else {
          prefix ++ plain("{ ").l ++ refinedElems.flatMap(e => parseRefinedElem(e.name, e.info)) ++ plain(" }").l
        }
      }
      case t @ AppliedType(tpe, typeList) =>
        import dotty.tools.dotc.util.Chars._
        if !t.typeSymbol.name.forall(isIdentifierPart) && typeList.size == 2 then
          inner(typeList.head)
          ++ plain(" ").l
          ++ inner(tpe)
          ++ plain(" ").l
          ++ inner(typeList.last)
        else if t.isFunctionType then
          typeList match
            case Nil =>
              Nil
            case Seq(rtpe) =>
              plain("()").l ++ keyword(" => ").l ++ inner(rtpe)
            case Seq(arg, rtpe) =>
              val partOfSignature = arg match
                case byName: ByNameType => plain("(").l ++ inner(byName) ++ plain(")").l
                case _ => inner(arg)
              partOfSignature ++ keyword(" => ").l ++ inner(rtpe)
            case args =>
              plain("(").l ++ commas(args.init.map(inner)) ++ plain(")").l ++ keyword(" => ").l ++ inner(args.last)
        else if t.isTupleN then
          typeList match
            case Nil =>
              Nil
            case args =>
              plain("(").l ++ commas(args.map(inner)) ++ plain(")").l
        else inner(tpe) ++ plain("[").l ++ commas(typeList.map { t => t match
          case _: TypeBounds => keyword("_").l ++ inner(t)
          case _ => inner(t)
        }) ++ plain("]").l

      case tp @ TypeRef(qual, typeName) =>
        qual match {
          case r: RecursiveThis => tpe(s"this.$typeName").l
          case _: TypeRepr => tpe(tp.typeSymbol)
        }
        // convertTypeOrBoundsToReference(reflect)(qual) match {
        //     case TypeReference(label, link, xs, _) => TypeReference(typeName, link + "/" + label, xs, true)
        //     case EmptyReference => TypeReference(typeName, "", Nil, true)
        //     case _ if tp.typeSymbol.exists =>
        //     tp.typeSymbol match {
        //         // NOTE: Only TypeRefs can reference ClassDefSymbols
        //         case sym if sym.isClassDef => //Need to be split because these types have their own file
        //         convertTypeOrBoundsToReference(reflect)(qual) match {
        //             case TypeReference(label, link, xs, _) => TypeReference(sym.name, link + "/" + label, xs, true)
        //             case EmptyReference if sym.name == "<root>" | sym.name == "_root_" => EmptyReference
        //             case EmptyReference => TypeReference(sym.name, "", Nil, true)
        //             case _ => throw Exception("Match error in SymRef/TypeOrBounds/ClassDef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        //         }

        //         // NOTE: This branch handles packages, which are now TypeRefs
        //         case sym if sym.isTerm || sym.isTypeDef =>
        //         convertTypeOrBoundsToReference(reflect)(qual) match {
        //             case TypeReference(label, link, xs, _) => TypeReference(sym.name, link + "/" + label, xs)
        //             case EmptyReference if sym.name == "<root>" | sym.name == "_root_" => EmptyReference
        //             case EmptyReference => TypeReference(sym.name, "", Nil)
        //             case _ => throw Exception("Match error in SymRef/TypeOrBounds/Other. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        //         }
        //         case sym => throw Exception("Match error in SymRef. This should not happen, please open an issue. " + sym)
        //     }
        //     case _ =>
        //     throw Exception("Match error in TypeRef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        // }
      case tr @ TermRef(qual, typeName) =>
        tr.termSymbol.tree match
          case vd: ValDef => inner(vd.tpt.tpe)
          case _          => tpe(tr.termSymbol)


        // convertTypeOrBoundsToReference(reflect)(qual) match {
        //     case TypeReference(label, link, xs, _) => TypeReference(typeName + "$", link + "/" + label, xs)
        //     case EmptyReference => TypeReference(typeName, "", Nil)
        //     case _ => throw Exception("Match error in TermRef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        // }

      // NOTE: old SymRefs are now either TypeRefs or TermRefs - the logic here needs to be moved into above branches
      // NOTE: _.symbol on *Ref returns its symbol
      // case SymRef(symbol, typeOrBounds) => symbol match {
      // }
      // case _ => throw Exception("No match for type in conversion to Reference. This should not happen, please open an issue. " + tp)
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

      case ParamRef(TypeLambda(names, _, _), i) => tpe(names.apply(i)).l

      case ParamRef(m: MethodType, i) => tpe(m.paramNames(i)).l

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

  private def typeBound(using Quotes)(t: reflect.TypeRepr, low: Boolean) =
    import reflect._
    val ignore = if (low) t.typeSymbol == defn.NothingClass else t.typeSymbol == defn.AnyClass
    val prefix = keyword(if low then " >: " else " <: ")
    t match {
      case l: TypeLambda => prefix :: plain("(").l ++ inner(l) ++ plain(")").l
      case p: ParamRef => prefix :: inner(p)
      case other if !ignore => prefix :: inner(other)
      case _ => Nil
    }

  private def typeBoundsTreeOfHigherKindedType(using Quotes)(low: reflect.TypeRepr, high: reflect.TypeRepr) =
    import reflect._
    def regularTypeBounds(low: TypeRepr, high: TypeRepr) =
      typeBound(low, low = true) ++ typeBound(high, low = false)
    high.match
      case TypeLambda(params, paramBounds, resType) =>
        if resType.typeSymbol == defn.AnyClass then
          plain("[").l ++ commas(params.zip(paramBounds).map { (name, typ) =>
            val normalizedName = if name.matches("_\\$\\d*") then "_" else name
            tpe(normalizedName).l ++ inner(typ)
          }) ++ plain("]").l
        else
          regularTypeBounds(low, high)
      case _ => regularTypeBounds(low, high)
