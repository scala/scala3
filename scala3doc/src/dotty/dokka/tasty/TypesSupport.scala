package dotty.dokka.tasty

import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.{Projection => JProjection}
import collection.JavaConverters._

trait TypesSupport:
  self: TastyParser =>
  import qctx.reflect._

  def getGivenInstance(method: DefDef): Option[Bound] = {
    def extractTypeSymbol(t: Tree): Option[Symbol] = t match
    case tpeTree: TypeTree =>
      inner(tpeTree.tpe)
    case other => None

    def inner(tpe: TypeRepr): Option[Symbol] = tpe match
    case ThisType(tpe) => inner(tpe)
    case AnnotatedType(tpe, _) => inner(tpe)
    case AppliedType(tpe, _) => inner(tpe)
    case tp @ TermRef(qual, typeName) =>
      qual match
      case _: TypeRepr | _: NoPrefix => Some(tp.termSymbol)
      case other => None
    case tp @ TypeRef(qual, typeName) =>
      qual match
      case _: TypeRepr | _: NoPrefix => Some(tp.typeSymbol)
      case other => None

    val typeSymbol = extractTypeSymbol(method.returnTpt)

    typeSymbol.map(_.tree).collect {
    case c: ClassDef => c.getParents.headOption
    case _ => Some(method.returnTpt)
    }.flatten.map(_.dokkaType)
  }

  given TreeSyntax as AnyRef:
    extension (tpeTree: Tree):
      def dokkaType: Bound =
        val data = tpeTree match
          case TypeBoundsTree(low, high) => typeBound(low.tpe, low = true) ++ typeBound(high.tpe, low = false)
          case tpeTree: TypeTree =>  inner(tpeTree.tpe)
          case term:  Term => inner(term.tpe)

        new GenericTypeConstructor(tpeTree.symbol.dri, data.asJava, null)

  given TypeSyntax as AnyRef:
    extension (tpe: TypeRepr):
      def dokkaType: Bound =
        val data = inner(tpe)
        val dri = data.collect{
          case o: TypeParameter => o
        }.headOption.map(_.getDri).getOrElse(defn.AnyClass.dri)
        new GenericTypeConstructor(dri, data.asJava, null)

  private def text(str: String): JProjection = new UnresolvedBound(str)

  private def texts(str: String): List[JProjection] = List(text(str))


  private def link(symbol: Symbol): List[JProjection] = {
    val suffix = if symbol.isValDef then texts(".type") else Nil
    (new TypeParameter(symbol.dri, symbol.normalizedName, null)) :: suffix
  }

  private def commas(lists: List[List[JProjection]]) = lists match
    case List(single) => single
    case other => other.reduce((r, e) => r ++ texts(", ") ++ e)

  private def isRepeated(tpeAnnotation: Term) =
    // For some reason annotation.tpe.typeSymbol != defn.RepeatedParamClass
    // annotation.tpe.typeSymbol prints 'class Repeated' and defn.RepeatedParamClass prints 'class <repeated>'
    tpeAnnotation.tpe.typeSymbol.toString == "class Repeated"

  // TODO #23 add support for all types signatures that makes sense
  private def inner(tp: TypeRepr): List[JProjection] =
    def noSupported(name: String): List[JProjection] =
      println(s"WARN: Unsupported type: $name: ${tp.show}")
      List(text(s"Unsupported[$name]"))

    tp match
      case OrType(left, right) => inner(left) ++ texts(" | ") ++ inner(right)
      case AndType(left, right) => inner(left) ++ texts(" & ") ++ inner(right)
      case ByNameType(tpe) => text("=> ") :: inner(tpe)
      case ConstantType(constant) =>
        texts(constant.value match
          case c: Char => s"'$c'"
          case other => other.toString
        )
      case ThisType(tpe) => inner(tpe)
      case AnnotatedType(AppliedType(_, Seq(tpe)), annotation) if isRepeated(annotation) =>
        inner(tpe) :+ text("*")
      case AnnotatedType(tpe, _) =>
        inner(tpe)
      case tl @ TypeLambda(params, paramBounds, resType) =>
        // println(params)
        // println(paramBounds)
        texts("[") ++ commas(params.zip(paramBounds).map( (name, typ) => texts(s"${name}") ++ inner(typ) )) ++ texts("]")
        ++ texts(" =>> ")
        ++ inner(resType)


      case r: Refinement => { //(parent, name, info)
        def getRefinementInformation(t: TypeRepr): List[TypeRepr] = t match {
          case r: Refinement => getRefinementInformation(r.parent) :+ r
          case t => List(t)
        }

        def getParamBounds(t: PolyType): List[JProjection] = commas(
          t.paramNames.zip(t.paramBounds.map(inner(_)))
            .map(b => texts(b(0)) ++ b(1))
        )

        def getParamList(m: MethodType): List[JProjection] =
          texts("(")
          ++ m.paramNames.zip(m.paramTypes).map{ case (name, tp) => texts(s"$name: ") ++ inner(tp)}
            .reduceLeftOption((acc: List[JProjection], elem: List[JProjection]) => acc ++ texts(", ") ++ elem).getOrElse(List())
          ++ texts(")")

        def parseRefinedElem(name: String, info: TypeRepr, polyTyped: List[JProjection] = Nil): List[JProjection] = ( info match {
          case m: MethodType => {
            val paramList = getParamList(m)
            texts(s"def $name") ++ polyTyped ++ paramList ++ texts(": ") ++ inner(m.resType)
          }
          case t: PolyType => {
            val paramBounds = getParamBounds(t)
            val parsedMethod = parseRefinedElem(name, t.resType)
            if (!paramBounds.isEmpty){
              parseRefinedElem(name, t.resType, texts("[") ++ paramBounds ++ texts("]"))
            } else parseRefinedElem(name, t.resType)
          }
          case ByNameType(tp) => texts(s"def $name: ") ++ inner(tp)
          case t: TypeBounds => texts(s"type $name") ++ inner(t)
          case t: TypeRef => texts(s"val $name: ") ++ inner(t)
          case t: TermRef => texts(s"val $name: ") ++ inner(t)
          case other => noSupported(s"Not supported type in refinement $info")
        } ) ++ texts("; ")

        def parsePolyFunction(info: TypeRepr): List[JProjection] = info match {
          case t: PolyType =>
            val paramBounds = getParamBounds(t)
            val method = t.resType.asInstanceOf[MethodType]
            val paramList = getParamList(method)
            val resType = inner(method.resType)
            texts("[") ++ paramBounds ++ texts("] => ") ++ paramList ++ texts(" => ") ++ resType
          case other => noSupported(s"Not supported type in refinement $info")
        }
        val refinementInfo = getRefinementInformation(r)
        val refinedType = refinementInfo.head
        val refinedElems = refinementInfo.tail.collect{ case r: Refinement => r }.toList
        val prefix = if refinedType.typeSymbol != defn.ObjectClass then inner(refinedType) ++ texts(" ") else List.empty[JProjection]
        if (refinedType.typeSymbol.fullName == "scala.PolyFunction" && refinedElems.size == 1) {
          parsePolyFunction(refinedElems.head.info)
        } else {
          prefix ++ texts("{ ") ++ refinedElems.flatMap(e => parseRefinedElem(e.name, e.info)) ++ texts(" }")
        }
      }
      case t @ AppliedType(tpe, typeList) =>
        import dotty.tools.dotc.util.Chars._
        if !t.typeSymbol.name.forall(isIdentifierPart) && typeList.size == 2 then
          inner(typeList.head)
          ++ texts(" ")
          ++ inner(tpe)
          ++ texts(" ")
          ++ inner(typeList.last)
        else if t.isFunctionType then
          typeList match
            case Nil =>
              Nil
            case Seq(rtpe) =>
              text("() => ") :: inner(rtpe)
            case Seq(arg, rtpe) =>
              inner(arg) ++ texts(" => ") ++ inner(rtpe)
            case args =>
              texts("(") ++ commas(args.init.map(inner)) ++ texts(") => ") ++ inner(args.last)
        else if t.isTupleType then
          typeList match
            case Nil =>
              Nil
            case args =>
              texts("(") ++ commas(args.map(inner)) ++ texts(")")
        else inner(tpe) ++ texts("[") ++ commas(typeList.map(inner)) ++ texts("]")

      case tp @ TypeRef(qual, typeName) =>
        qual match {
          case r: RecursiveThis => texts(s"this.$typeName")
          case _: TypeRepr | _: NoPrefix => link(tp.typeSymbol)
          case other => noSupported(s"TypeRepr: $tp")
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
      case tr @ TermRef(qual, typeName) => qual match {
        case _ => link(tr.termSymbol)
      }
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
        if(low == hi) texts(" = ") ++ inner(low)
        else typeBound(low, low = true) ++ typeBound(hi, low = false)

      case NoPrefix() => Nil

      case MatchType(bond, sc, cases) =>
        val casesTexts = cases.flatMap {
          case MatchTypeCase(from, to) =>
            texts("  case ") ++ inner(from) ++ texts(" => ") ++ inner(to) ++ texts("\n")
        }
        inner(sc) ++ texts(" match {\n") ++ casesTexts ++ texts("}")

      case ParamRef(TypeLambda(names, _, _), i) => texts(names.apply(i))
      
      case ParamRef(m: MethodType, i) => texts(m.paramNames(i))

      case RecursiveType(tp) => inner(tp)

  private def typeBound(t: TypeRepr, low: Boolean) =
    val ignore = if(low) t.typeSymbol == defn.NothingClass  else t.typeSymbol == defn.AnyClass
    val prefix = text(if low then " >: " else " <: ")
    t match {
      case l: TypeLambda => prefix :: texts("(") ++ inner(l) ++ texts(")")
      case p: ParamRef => prefix :: inner(p)
      case other if !ignore => prefix :: inner(other)
      case _ => Nil
    }

