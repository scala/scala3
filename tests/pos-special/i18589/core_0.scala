import scala.deriving.Mirror

trait NamedCodec[A, R]

object NamedCodecPlatform {

  final class Builder[R]() {
    inline def of[T](using m: Mirror.Of[T]): NamedCodec[T, R] =
      inline m match {
        case s: Mirror.SumOf[T]     => sumInst(s)
        case _: Mirror.ProductOf[T] => productInst
      }

    private inline def productInst[T]: NamedCodec[T, R] = ???
    private inline def sumInst[T](m: Mirror.SumOf[T]): NamedCodec[T, R] = ???
  }
}
