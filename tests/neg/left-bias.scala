//> using options -Yexplicit-nulls
import language.experimental.errorHandling

// Higher-kinded type inference for `Maybe` is left-biased: matching `F[_]`
// against `T ? E` infers `F := [X] =>> X ? E`. Code that relies on abstracting
// over the error type instead no longer typechecks without explicit type arguments.

def foo[F[_]](x: F[Int]): F[Boolean] = ???

def errorPosition(x: String ? Int) =
  val _: String ? Boolean = foo(x) // error

def optionalError(x: String ? Int) =
  val _: String? = foo(x) // error

// With left bias the value type must be the one that varies
def extract[F[_], A](x: F[A]): A = ???

def extracted(x: Int ? String) =
  val _: String = extract(x) // error

// Arguments must share the same error type
def zip[F[_], A, B](x: F[A], y: F[B]): F[(A, B)] = ???

def zipped(x: Int ? String, y: Int ? Boolean) =
  val _: (Int, Int) ? String = zip(x, y) // error

// Bounds on the type constructor parameter apply to the value type
def fooBounded[F[_ <: AnyVal]](x: F[Int]): F[Boolean] = ???

def bounded(x: String ? Int) =
  fooBounded(x) // error

// A type class instance that abstracts over the error type is not found
trait Functor[F[_]]:
  extension [A](x: F[A]) def fmap[B](f: A => B): F[B]

given rightFunctor: [T] => Functor[[X] =>> T ? X]:
  extension [A](x: T ? A) def fmap[B](f: A => B): T ? B = ???

def fmapTwice[F[_]: Functor, A](x: F[A])(f: A => A): F[A] =
  x.fmap(f).fmap(f)

def functor(x: String ? Int) =
  fmapTwice(x)(_ + 1) // error
