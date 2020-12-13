import scala.deriving._
import scala.compiletime.erasedValue

trait JsonEncoder[T] {
  def encode(elem: T): String
}

object JsonEncoder {
  import scala.compiletime.{erasedValue, summonInline}
  import compiletime._
  import scala.deriving._

  inline def encodeElem[T](elem: T): String = summonInline[JsonEncoder[T]].encode(elem)

  inline def encodeElems[Elems <: Tuple](idx: Int)(value: Product): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        encodeElem[elem](value.productElement(idx).asInstanceOf[elem]) :: encodeElems[elems1](idx + 1)(value)
      case _ => Nil
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): JsonEncoder[T] = new JsonEncoder[T] {
    def encode(value: T): String =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          "not supporting this case yet"
        case m: Mirror.ProductOf[T] =>
          val valueProduct = value.asInstanceOf[Product]
          val elems = encodeElems[m.MirroredElemTypes](0)(valueProduct)
          val labels = valueProduct.productElementNames
          val keyValues = labels.zip(elems).map((k, v) => s"$k: $v")
          "{" + (keyValues).mkString(", ") + "}"
        case other =>
          throw new RuntimeException("mirror was an invalid value: " + other)
      }
  }

  given listEncoder[T](using encoder: JsonEncoder[T]): JsonEncoder[List[T]] with {
    def encode(list: List[T]) = s"[${ list.map(v => encoder.encode(v)).mkString(", ") }]"
  }

  given intEncoder: JsonEncoder[Int] with {
    def encode(value: Int) = value + ""
  }

  given stringEncoder: JsonEncoder[String] with {
    def encode(value: String) = value
  }
}
