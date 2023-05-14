import scala.deriving.Mirror
import scala.compiletime._

trait Schema[T]

object Schema {
  implicit val stringSchema: Schema[String] = new Schema[String] {}
  implicit def listSchema[A](implicit ev: Schema[A]): Schema[List[A]] = new Schema[List[A]] {}
  implicit def mapSchema[A, B](implicit evA: Schema[A], evB: Schema[B]): Schema[Map[A, B]] =
    new Schema[Map[A, B]] {}

  inline def recurse[Label, A <: Tuple](index: Int = 0): List[(String, Schema[Any], Int)] =
    inline erasedValue[(Label, A)] match {
      case (_: (name *: names), _: (t *: ts)) =>
        val label       = constValue[name].toString
        val builder     = summonInline[Schema[t]].asInstanceOf[Schema[Any]]
        (label, builder, index) :: recurse[names, ts](index + 1)
      case (_: EmptyTuple, _)                 => Nil
    }

  inline def derived[A]: Schema[A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A]     =>
        lazy val members     = recurse[m.MirroredElemLabels, m.MirroredElemTypes]()
        new Schema[A] {}
      case m: Mirror.ProductOf[A] =>
        lazy val fields           = recurse[m.MirroredElemLabels, m.MirroredElemTypes]()
        new Schema[A] {}
    }

  inline given gen[A]: Schema[A] = derived[A]
}

sealed trait InputValue
object InputValue {
  case class ListValue(values: List[InputValue])          extends InputValue
  case class ObjectValue(fields: Map[String, InputValue]) extends InputValue
  case class VariableValue(name: String)                  extends InputValue
}

@main def Test =
  implicit lazy val inputValueSchema: Schema[InputValue] = Schema.gen
  println(summon[Schema[InputValue]])