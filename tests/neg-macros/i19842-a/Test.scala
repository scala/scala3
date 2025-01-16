
trait Serializer[@specialized T]

object Serializer:
  inline given [T] => Serializer[T] = ${ Macros.makeSerializer[T] }

case class ValidationCls(string: String)

@main def Test = summon[Serializer[ValidationCls]] // error
