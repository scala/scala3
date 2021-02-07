class Encoder[T] { def encode(v: T): String = v.toString }
case class ValueWithEncoder[T](value: T, encoder: Encoder[T])

object Test {
  val a: Seq[ValueWithEncoder[_]] = Seq.empty
  val b = a.map(ve => ve.encoder.encode(ve.value))
  val c: Seq[String] = b
}
