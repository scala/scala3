package scala

import scala.quoted.*

object TupleMacros {

  /** Macro implementation for type-preserving Tuple construction.
   *  Extracts the precise types of each argument and builds a properly typed TupleN.
   */
  def applyImpl(args: Expr[Seq[Any]])(using Quotes): Expr[Tuple] = {
    import quotes.reflect.*

    def extractRepeated(term: Term): List[Term] = term match {
      case Repeated(elems, _) => elems
      case Inlined(_, _, inner) => extractRepeated(inner)
      case Typed(inner, _) => extractRepeated(inner)
      case Block(_, inner) => extractRepeated(inner)
      case _ =>
        report.error(s"Expected literal varargs, got: ${term.show}")
        Nil
    }

    val argTerms = extractRepeated(args.asTerm)
    val n = argTerms.length

    n match {
      case 0 => '{ EmptyTuple }
      case _ if n <= scala.runtime.Tuples.MaxSpecialized =>
        val tupleClass = Symbol.requiredClass(s"scala.Tuple$n")
        val tupleType = tupleClass.typeRef.appliedTo(argTerms.map(_.tpe.widen))
        val tupleCompanion = tupleClass.companionModule

        val applyMethod = tupleCompanion.methodMember("apply").head
        val call = Apply(
          TypeApply(
            Select(Ref(tupleCompanion), applyMethod),
            argTerms.map(t => TypeTree.of(using t.tpe.widen.asType.asInstanceOf[Type[Any]]))
          ),
          argTerms
        )

        tupleType.asType match {
          case '[t] => call.asExprOf[t & Tuple]
        }

      case _ =>
        val consType = argTerms.foldRight(TypeRepr.of[EmptyTuple]) { (term, acc) =>
          TypeRepr.of[*:].appliedTo(List(term.tpe.widen, acc))
        }

        val arrayElements = argTerms.map { t =>
          t.tpe.widen.asType match {
            case '[elem] =>
              val termExpr = t.asExprOf[elem]
              '{ $termExpr.asInstanceOf[Object] }
          }
        }
        val arrayExpr = Expr.ofSeq(arrayElements)

        consType.asType match {
          case '[t] =>
            '{ scala.runtime.Tuples.fromArray($arrayExpr.toArray[Object]).asInstanceOf[t & Tuple] }
        }
    }
  }
}
