
trait Serializer[@specialized T]

object Serializer:
    implicit inline def implicitMakeSerializer[T]: Serializer[T] = ${ Macros.makeSerializer[T] }

case class ValidationCls(string: String)

@main def Test = summon[Serializer[ValidationCls]] // error