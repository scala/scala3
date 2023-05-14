import scala.quoted.*
import scala.deriving.Mirror

// derivation code is a slightly modified version of: https://github.com/lampepfl/dotty-macro-examples/blob/main/macroTypeClassDerivation/src/macro.scala
object Derivation {

  // Typeclass instance gets constructed as part of a macro
  inline given deriveFullyConstrucedByMacro[A](using Mirror.ProductOf[A]): Show[A] = Derivation.deriveShow[A]

  // Typeclass instance is built inside as part of a method, only the 'show' impl is filled in by a macro
  inline given derivePartiallyConstructedByMacro[A](using Mirror.ProductOf[A]): Show[A] =
    new {
      def show(value: A): String = Derivation.show(value)
    }

  inline def show[T](value: T): String = ${ showValue('value) }

  inline def deriveShow[T]: Show[T] = ${ deriveCaseClassShow[T] }

  private def deriveCaseClassShow[T](using quotes: Quotes, tpe: Type[T]): Expr[Show[T]] = {
    import quotes.reflect.*
    // Getting the case fields of the case class
    val fields: List[Symbol] = TypeTree.of[T].symbol.caseFields

    '{
      new Show[T] {
        override def show(t: T): String =
          ${ showValue('t) }
      }
    }
  }

  def showValue[T: Type](value: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect.*

    val fields: List[Symbol] = TypeTree.of[T].symbol.caseFields

    val vTerm: Term = value.asTerm
    val valuesExprs: List[Expr[String]] = fields.map(showField(vTerm, _))
    val exprOfList: Expr[List[String]] = Expr.ofList(valuesExprs)
    '{ "{ " + $exprOfList.mkString(", ") + " }" }
  }

  /** Create a quoted String representation of a given field of the case class */
  private def showField(using Quotes)(caseClassTerm: quotes.reflect.Term, field: quotes.reflect.Symbol): Expr[String] = {
    import quotes.reflect.*

    val fieldValDef: ValDef = field.tree.asInstanceOf[ValDef]
    val fieldTpe: TypeRepr = fieldValDef.tpt.tpe
    val fieldName: String = fieldValDef.name

    val tcl: Term = lookupShowFor(fieldTpe) // Show[$fieldTpe]
    val fieldValue: Term = Select(caseClassTerm, field) // v.field
    val strRepr: Expr[String] = applyShow(tcl, fieldValue).asExprOf[String]
    '{ ${ Expr(fieldName) } + ": " + $strRepr } // summon[Show[$fieldTpe]].show(v.field)
  }

  /** Look up the Show[$t] typeclass for a given type t */
  private def lookupShowFor(using Quotes)(t: quotes.reflect.TypeRepr): quotes.reflect.Term = {
    import quotes.reflect.*
    t.asType match {
      case '[tpe] =>
        Implicits.search(TypeRepr.of[Show[tpe]]) match {
          case res: ImplicitSearchSuccess   => res.tree
          case failure: DivergingImplicit   => report.errorAndAbort(s"Diverving: ${failure.explanation}")
          case failure: NoMatchingImplicits => report.errorAndAbort(s"NoMatching: ${failure.explanation}")
          case failure: AmbiguousImplicits  => report.errorAndAbort(s"Ambiguous: ${failure.explanation}")
          case failure: ImplicitSearchFailure =>
            report.errorAndAbort(s"catch all: ${failure.explanation}")
        }
    }
  }

  /** Composes the tree: $tcl.show($arg) */
  private def applyShow(using Quotes)(tcl: quotes.reflect.Term, arg: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect.*
    Apply(Select.unique(tcl, "show"), arg :: Nil)
  }
}
