trait Schema[A]
object Schema:
    final case class Either[A, B](left: Schema[A], right: Schema[B]) extends Schema[scala.util.Either[A, B]]
trait DecodeError

object DynamicValue:
  final case class LeftValue(value: DynamicValue) extends DynamicValue
  final case class RightValue(value: DynamicValue) extends DynamicValue

sealed trait DynamicValue:
  self =>
    def toTypedValueLazyError[A](using schema: Schema[A]): Either[DecodeError, A] =
      (self, schema) match
        case (DynamicValue.LeftValue(value), Schema.Either(schema1, _)) =>
          value.toTypedValueLazyError(using schema1).map(Left(_))
        case (DynamicValue.RightValue(value), Schema.Either(_, schema1)) =>
          value.toTypedValueLazyError(using schema1).map(Right(_))
        case _ => ???