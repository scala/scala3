import scala.quoted.*

trait UnWrapper {
  type Type
  type Underlying

  def unwrap(value: Type): Underlying
}

object UnWrapper {
  type Of[T] = UnWrapper { type Type = T }

  transparent inline given derived[T]: UnWrapper.Of[T] = ${ wrapUnwrapImpl[T] }

  private def wrapUnwrapImpl[T: Type](using Quotes): Expr[UnWrapper.Of[T]] = {
    import quotes.reflect.*
    val symbol = TypeRepr.of[T].typeSymbol
    symbol.caseFields match {
      case field :: Nil =>
        field.termRef.widen.asType match {
          case '[fieldType] =>
            '{
              new UnWrapper {
                type Type = T
                type Underlying = fieldType

                def unwrap(value: T): fieldType = ${ '{ value }.asTerm.select(field).asExprOf[fieldType] }
              }
            }
        }
      case _ => sys.error("single case field expected")
    }
  }
}
