object K0 {

  type T = String

  opaque type ProductInstances[F[_], T] = ErasedProductInstances[F[T]]

  inline def summonAsArray[F[_], T]: Array[Any] = ???
  inline def mkProductInstances[F[_], T]: ProductInstances[F, T] =
    new ErasedProductInstances(summonAsArray[F, T]).asInstanceOf[ProductInstances[F, T]]

  val x: T = ""

  inline def foo(x: T): T = "foo".asInstanceOf[T]
}

final class ErasedProductInstances[FT](is0: => Array[Any])

trait Monoid[A]
case class ISB(i: Int)

object Test {
  K0.foo(K0.x)
  K0.mkProductInstances[Monoid, ISB]

}