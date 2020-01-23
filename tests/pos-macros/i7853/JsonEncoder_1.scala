import scala.deriving._
import scala.compiletime.erasedValue

trait JsonEncoder[T] {
  def encode(elem: T): String
}

object JsonEncoder {
  import scala.compiletime.{erasedValue, summonFrom}
  import compiletime._
  import scala.deriving._

  inline def encodeElem[T](elem: T): String = summonFrom {
    case encoder: JsonEncoder[T] => encoder.encode(elem)
  }

  inline def encodeElems[Elems <: Tuple](idx: Int)(value: Any): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        encodeElem[elem](productElement[elem](value, idx)) :: encodeElems[elems1](idx + 1)(value)
      case _ => Nil
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): JsonEncoder[T] = new JsonEncoder[T] {
    def encode(value: T): String =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          "not supporting this case yet"
        case m: Mirror.ProductOf[T] =>
          val elems = encodeElems[m.MirroredElemTypes](0)(value)
          val labels = value.asInstanceOf[Product].productElementNames
          val keyValues = labels.zip(elems).map((k, v) => s"$k: $v")
          "{" + (keyValues).mkString(", ") + "}"
        case other =>
          throw new RuntimeException("mirror was an invalid value: " + other)
      }
  }

  given listEncoder[T] with (encoder: JsonEncoder[T]) as JsonEncoder[List[T]] {
    def encode(list: List[T]) = s"[${ list.map(v => encoder.encode(v)).mkString(", ") }]"
  }

  given intEncoder as JsonEncoder[Int] {
    def encode(value: Int) = value + ""
  }

  given stringEncoder as JsonEncoder[String] {
    def encode(value: String) = value
  }
}