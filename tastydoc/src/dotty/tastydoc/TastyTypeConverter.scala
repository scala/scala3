package dotty.tastydoc

import scala.quoted._
import dotty.tastydoc.references._

/** Trait containing methods for converting from Reflect types to References */
trait TastyTypeConverter {

  def convertTypeToReference(using QuoteContext)(tp: qctx.tasty.Type): Reference = {
    import qctx.tasty._

    //Inner method to avoid passing the reflection each time
    def inner(tp: Type): Reference = tp match {
      case OrType(left, right) => OrTypeReference(inner(left), inner(right))
      case AndType(left, right) => AndTypeReference(inner(left), inner(right))
      case ByNameType(tpe) => ByNameReference(inner(tpe))
      case ConstantType(constant) => ConstantReference(constant.value.toString)
      case ThisType(tpe) => inner(tpe)
      case AnnotatedType(tpe, _) => inner(tpe)
      case TypeLambda(paramNames, paramTypes, resType) => ConstantReference(tp.show) //TOFIX
      case Refinement(parent, name, info) =>
        val tuple = convertTypeToReference(info) match {
          case r if (info match {case info: TypeBounds => true case _ => false}) => ("type", name, r)
          case r@TypeReference(_, _, _, _) => ("val", name, r)
          case ByNameReference(rChild) => ("def", name, rChild)
          case r => throw new Exception("Match error in info of Refinement. This should not happen, please open an issue. " + r)
        }
        convertTypeToReference(parent) match {
          case RefinedReference(p, ls) =>
            RefinedReference(p, ls:+tuple)
          case t => RefinedReference(t, List(tuple))
        }
      case AppliedType(tpe, typeOrBoundsList) =>
        inner(tpe) match {
          case TypeReference(label, link, _, hasOwnFile) =>
            if(link == "/scala"){
              if(label.matches("Function[1-9]") || label.matches("Function[1-9][0-9]")){
                val argsAndReturn = typeOrBoundsList.map(convertTypeToReference(_))
                FunctionReference(argsAndReturn.take(argsAndReturn.size - 1), argsAndReturn.last, false)
              }else if(label.matches("Tuple[1-9]") || label.matches("Tuple[1-9][0-9]")){
                TupleReference(typeOrBoundsList.map(convertTypeToReference(_)))
              }else{
                TypeReference(label, link, typeOrBoundsList.map(convertTypeToReference(_)), hasOwnFile)
              }
            }else{
              TypeReference(label, link, typeOrBoundsList.map(convertTypeToReference(_)), hasOwnFile)
            }
          case _ => throw Exception("Match error in AppliedType. This should not happen, please open an issue. " + tp)
        }
      case tp @ TypeRef(qual, typeName) =>
        convertTypeToReference(qual) match {
          case TypeReference(label, link, xs, _) => TypeReference(typeName, link + "/" + label, xs, true)
          case EmptyReference => TypeReference(typeName, "", Nil, true)
          case _ if tp.typeSymbol.exists =>
            tp.typeSymbol match {
              // NOTE: Only TypeRefs can reference ClassDefSymbols
              case sym if sym.isClassDef => //Need to be split because these types have their own file
                convertTypeToReference(qual) match {
                  case TypeReference(label, link, xs, _) => TypeReference(sym.name, link + "/" + label, xs, true)
                  case EmptyReference if sym.name == "<root>" | sym.name == "_root_" => EmptyReference
                  case EmptyReference => TypeReference(sym.name, "", Nil, true)
                  case _ => throw Exception("Match error in SymRef/Types/ClassDef. This should not happen, please open an issue. " + convertTypeToReference(qual))
                }

              // NOTE: This branch handles packages, which are now TypeRefs
              case sym if sym.isTerm || sym.isTypeDef =>
                convertTypeToReference(qual) match {
                  case TypeReference(label, link, xs, _) => TypeReference(sym.name, link + "/" + label, xs)
                  case EmptyReference if sym.name == "<root>" | sym.name == "_root_" => EmptyReference
                  case EmptyReference => TypeReference(sym.name, "", Nil)
                  case _ => throw Exception("Match error in SymRef/Types/Other. This should not happen, please open an issue. " + convertTypeToReference(qual))
                }
              case sym => throw Exception("Match error in SymRef. This should not happen, please open an issue. " + sym)
            }
          case _ =>
            throw Exception("Match error in TypeRef. This should not happen, please open an issue. " + convertTypeToReference(qual))
        }
      case TermRef(qual, typeName) =>
        convertTypeToReference(qual) match {
          case TypeReference(label, link, xs, _) => TypeReference(typeName + "$", link + "/" + label, xs)
          case EmptyReference => TypeReference(typeName, "", Nil)
          case _ => throw Exception("Match error in TermRef. This should not happen, please open an issue. " + convertTypeToReference(qual))
        }
      case TypeBounds(low, hi) =>
        val lowRef = convertTypeToReference(low)
        val hiRef = convertTypeToReference(hi)
        if(hiRef == lowRef){
          hiRef
        }else{
          BoundsReference(lowRef, hiRef)
        }
      case NoPrefix() => EmptyReference

      // NOTE: old SymRefs are now either TypeRefs or TermRefs - the logic here needs to be moved into above branches
      // NOTE: _.symbol on *Ref returns its symbol
      // case SymRef(symbol, typeOrBounds) => symbol match {
      // }
      // case _ => throw Exception("No match for type in conversion to Reference. This should not happen, please open an issue. " + tp)
    }

    inner(tp)
  }
}
