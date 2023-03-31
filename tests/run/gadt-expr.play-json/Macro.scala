sealed trait JsValue
final case class JsNumber(value: Int)                       extends JsValue
final case class JsObject(underlying: Map[String, JsValue]) extends JsValue

sealed trait JsResult[+A]
final case class JsSuccess[T](value: T) extends JsResult[T]
case object JsError                     extends JsResult[Nothing]

trait Reads[A]:
  def reads(json: JsValue): JsResult[A]
object Reads:
  given Reads[Int] = { case JsNumber(n) => JsSuccess(n) case _ => JsError }

object Macro:
  import scala.compiletime.*, scala.deriving.*

  inline def reads[A](using m: Mirror.ProductOf[A]): Reads[A] = new Reads[A]:
    def reads(js: JsValue) = js match
      case obj @ JsObject(_) =>
        val elems = new Array[Any](constValue[Tuple.Size[m.MirroredElemTypes]])
        rec[A, m.MirroredElemLabels, m.MirroredElemTypes](obj, elems)(0)
      case _ => JsError

  inline def rec[A, L <: Tuple, T <: Tuple](
      obj: JsObject, elems: Array[Any])(n: Int)(using m: Mirror.ProductOf[A]): JsResult[A] =
    inline (erasedValue[L], erasedValue[T]) match
      case _: (EmptyTuple, EmptyTuple) => JsSuccess(m.fromProduct(ArrayProduct(elems)))
      case _: (l *: ls, t *: ts) =>
        val key   = inline erasedValue[l] match { case s: String => s }
        val reads = summonInline[Reads[t]]
        reads.reads(obj.underlying(key)) match
          case JsSuccess(x) =>
            elems(n) = x
            rec[A, ls, ts](obj, elems)(n + 1)
          case _ => JsError

final class ArrayProduct[A](elems: Array[A]) extends Product:
  def canEqual(that: Any): Boolean  = that.isInstanceOf[ArrayProduct[_]]
  def productArity: Int             = elems.size
  def productElement(idx: Int): Any = elems(idx)
