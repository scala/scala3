trait ExMap1[K1, +V1] extends PartialFunction[K1, V1]
trait ExMap2[K2, +V2] extends PartialFunction[K2, V2]

trait Gen[L[_]] { def make: L[Unit] }

trait Functor[M[_]]:
  def map[A, B](ma: M[A])(f: A => B): M[B]
object Functor:
  implicit def inst1[K3]: Functor[[V3] =>> ExMap1[K3, V3]] = ???
  implicit def inst2[K4]: Functor[[V4] =>> ExMap2[K4, V4]] = ???

class Test:
  def foo(x: Unit): String = x.toString()
  def foo[F[_]](using F: Functor[F], G: Gen[F]): F[String] =
    val res1: F[String] = F.map[Unit, String](G.make)(foo)    // was: error
    val res2: F[String] = F.map[Unit, String](G.make)(foo(_)) // was: ok

    ??? : F[String]
