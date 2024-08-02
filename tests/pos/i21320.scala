import scala.deriving.*
import scala.compiletime.*

trait ConfigMonoid[T]:
  def zero: T
  def orElse(main: T, defaults: T): T

object ConfigMonoid:
  given option[T]: ConfigMonoid[Option[T]] = ???

  inline def zeroTuple[C <: Tuple]: Tuple =
    inline erasedValue[C] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts) =>
        summonInline[ConfigMonoid[t]].zero *: zeroTuple[ts]

  inline def valueTuple[C <: Tuple, T](index: Int, main: T, defaults: T): Tuple =
    inline erasedValue[C] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts) =>
        def get(v: T) = v.asInstanceOf[Product].productElement(index).asInstanceOf[t]
        summonInline[ConfigMonoid[t]].orElse(get(main), get(defaults)) *: valueTuple[ts, T](
          index + 1,
          main,
          defaults
        )

  inline given derive[T](using m: Mirror.ProductOf[T]): ConfigMonoid[T] =
    new ConfigMonoid[T]:
      def zero: T = m.fromProduct(zeroTuple[m.MirroredElemTypes])
      def orElse(main: T, defaults: T): T = m.fromProduct(valueTuple[m.MirroredElemTypes, T](0, main, defaults))



final case class PublishOptions(
  v1: Option[String] = None,
  v2: Option[String] = None,
  v3: Option[String] = None,
  v4: Option[String] = None,
  v5: Option[String] = None,
  v6: Option[String] = None,
  v7: Option[String] = None,
  v8: Option[String] = None,
  v9: Option[String] = None,
  ci: PublishContextualOptions = PublishContextualOptions(),
)
object PublishOptions:
  implicit val monoid: ConfigMonoid[PublishOptions] = ConfigMonoid.derive

final case class PublishContextualOptions(
  v1: Option[String] = None,
  v2: Option[String] = None,
  v3: Option[String] = None,
  v4: Option[String] = None,
  v5: Option[String] = None,
  v6: Option[String] = None,
  v7: Option[String] = None,
  v8: Option[String] = None,
  v9: Option[String] = None,
  v10: Option[String] = None,
  v11: Option[String] = None,
  v12: Option[String] = None,
  v13: Option[String] = None,
  v14: Option[String] = None,
  v15: Option[String] = None,
  v16: Option[String] = None,
  v17: Option[String] = None,
  v18: Option[String] = None,
  v19: Option[String] = None,
  v20: Option[String] = None
)
object PublishContextualOptions:
  implicit val monoid: ConfigMonoid[PublishContextualOptions] = ConfigMonoid.derive