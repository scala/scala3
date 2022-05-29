import scala.reflect.ClassTag
// https://github.com/json4s/json4s/blob/355d8751391773e0d79d04402a4f9fb7bfc684ec/ext/src/main/scala-3/org/json4s/ext/package.scala#L4-L8
type Aux[A] = { type Value = A }
type EnumValue[A <: Enumeration] = A match {
  case Aux[a] => a
}

// https://github.com/json4s/json4s/blob/355d8751391773e0d79d04402a4f9fb7bfc684ec/ext/src/main/scala/org/json4s/ext/EnumSerializer.scala#L25-L26
class EnumSerializer[E <: Enumeration: ClassTag](enumeration: E) {
  val EnumerationClass = classOf[EnumValue[E]] // error
}