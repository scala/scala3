import scala.deriving.*
import scala.compiletime.*

trait StringConverter[T]:
  def to(t: T): String

trait QueryParams
trait QueryParamsConverter[T]:
  def to(t: T): QueryParams

object QueryParamsConverter {
  extension [T](t: T)
    def toQueryParams(using converter: QueryParamsConverter[T]): QueryParams = converter.to(t)

  inline private def summonStringConverter[T]: StringConverter[T] = summonFrom {
    case converter: StringConverter[T] => converter
    case _ =>
      new StringConverter[T]: // warn
        override def to(t: T): String = t.toString
  }

  inline def summonStringConverters[A <: Tuple]: List[StringConverter[?]] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonStringConverter[t] :: summonStringConverters[ts]

  inline def summonQueryParamsConverters[A <: Tuple]: List[QueryParamsConverter[?]] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[QueryParamsConverter[t]] :: summonQueryParamsConverters[ts]

  inline given derived[T](using m: Mirror.Of[T]): QueryParamsConverter[T] = {
    val stringConverters = summonStringConverters[m.MirroredElemTypes]
    new QueryParamsConverter[T] { // warn
      override def to(t: T): QueryParams =
        inline m match
          case p: Mirror.ProductOf[T] => ???
          case s: Mirror.SumOf[T] =>
            val _ = summonQueryParamsConverters[m.MirroredElemTypes]
            ???
    }
  }

}

object Test:
  sealed trait FutureOrderCreateParams
  object FutureOrderCreateParams:
    case class LIMIT(quantity: BigDecimal) extends FutureOrderCreateParams

  case class FutureOrderCreateResponse(clientOrderId: String)

  import QueryParamsConverter.*
  def createOrder(orderCreate: FutureOrderCreateParams) = orderCreate.toQueryParams
