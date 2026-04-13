import scala.compiletime.summonFrom

trait Decoder[A]

given Decoder[String] with {}

trait Mirror[T]:
  type IronType

given [T](using mirror: Mirror[T], ev: Decoder[mirror.IronType]): Decoder[T] =
  ev.asInstanceOf[Decoder[T]]

object Foo:
  opaque type T = String

  inline given Mirror[T] with
    type IronType = String

inline def summonFooDecoder: Decoder[Foo.T] =
  summonFrom {
    case decodeA: Decoder[Foo.T] => decodeA
  }

val fooDecoder: Decoder[Foo.T] = summonFooDecoder
