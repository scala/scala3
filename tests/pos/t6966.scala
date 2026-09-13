import Ordering.{Byte, comparatorToOrdering}
trait Format[T]
trait InputCache[T]
object CacheIvy {
    implicit def basicInputCache[I](implicit fmt: Format[I], eqv: Equiv[I]): InputCache[I] = ???
    implicit def arrEquiv[T](implicit t: Equiv[T]): Equiv[Array[T]] = ???
    implicit def hNilCache: InputCache[HNil] = ???
    implicit def ByteArrayFormat: Format[Array[Byte]] = ???
    type :+:[H, T <: HList] = HCons[H,T]
    implicit def hConsCache[H, T <: HList](implicit head: InputCache[H], tail: InputCache[T]): InputCache[H :+: T] = ???
    hConsCache[Array[Byte], HNil]
}

sealed trait HList
sealed trait HNil extends HList
object HNil extends HNil
final class HCons[H, T <: HList](head : H, tail : T) extends HList
