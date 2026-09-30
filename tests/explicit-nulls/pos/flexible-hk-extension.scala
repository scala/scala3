// A higher-kinded extension method `map` (as provided e.g. by cats' `Functor`
// syntax together with a `Functor[Id]` instance) must not be applicable to a
// flexible-typed receiver such as the Java array `Throwable.getStackTrace`
// (`(Array[(StackTraceElement)?])?`). A flexible type `T?` is `=:=` to `T` and
// its type constructor is `=:=` to the identity, so without special handling the
// `F[A]`-shaped extension would match via the `Id` instance and shadow the
// intended `ArrayOps.map`, binding the lambda parameter to the whole array
// instead of its element. See the gcp4s community-build regression.
object catslike:
  trait Functor[F[_]]:
    extension [A](fa: F[A]) def fmap[B](f: A => B): F[B]
  type Id[A] = A
  given Functor[Id] with
    extension [A](fa: A) def fmap[B](f: A => B): B = f(fa)
  extension [F[_], A](fa: F[A])(using F: Functor[F]) def map[B](f: A => B): F[B] = F.fmap(fa)(f)

object Test:
  import catslike.*

  def stackFrames(t: Throwable): Option[List[String]] =
    Option(t.getStackTrace).map { st =>
      // `st.map` must resolve to `ArrayOps.map` (via the `refArrayOps` conversion),
      // so `ste` is a `StackTraceElement`, not the whole array.
      st.map { ste => ste.getMethodName }.toList
    }
