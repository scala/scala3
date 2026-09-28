//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.Ok

// Higher-kinded type inference for `Maybe` is left-biased: matching `F[_]`
// against `T ? E` infers `F := [X] =>> X ? E`, not `[X] =>> T ? X`.

def foo[F[_]](x: F[Int]): F[Boolean] = ???

def bar(x: Int ? String) =
  val y = foo(x)
  val _: Boolean ? String = y

// Optional types `T?`, i.e. `T ? Unit`
def optional(x: Int?) =
  val y = foo(x)
  val _: Boolean? = y

// A fixed error type that is itself a type application
def nestedError(x: Int ? List[String]) =
  val _: Boolean ? List[String] = foo(x)

// The value type can be a type application too
def nestedValue(x: List[Int] ? String) =
  def baz[F[_], A](x: F[List[A]]): F[A] = ???
  val _: Int ? String = baz(x)

// Nested maybe types: the outer `Maybe` is abstracted
def nestedMaybe(x: (Int ? String) ? Boolean) =
  def baz[F[_], A](x: F[A]): F[List[A]] = ???
  val _: List[Int ? String] ? Boolean = baz(x)

// Covariant type constructor parameter
def fooCov[F[+_]](x: F[Int]): F[Boolean] = ???

def cov(x: Int ? String) =
  val _: Boolean ? String = fooCov(x)

// Type constructor parameter with an upper bound on its argument
def fooBounded[F[_ <: AnyVal]](x: F[Int]): F[Boolean] = ???

def bounded(x: Int ? String) =
  val _: Boolean ? String = fooBounded(x)

// Result type is inferred from the argument's element type
def extract[F[_], A](x: F[A]): A = ???

def extracted(x: Int ? String) =
  val _: Int = extract(x)

// Several arguments sharing the same inferred type constructor
def zip[F[_], A, B](x: F[A], y: F[B]): F[(A, B)] = ???

def zipped(x: Int ? String, y: Boolean ? String) =
  val _: (Int, Boolean) ? String = zip(x, y)

// Type classes over the value type
trait Functor[F[_]]:
  extension [A](x: F[A]) def fmap[B](f: A => B): F[B]

given maybeFunctor: [E] => Functor[[X] =>> X ? E]:
  extension [A](x: A ? E) def fmap[B](f: A => B): B ? E = x.map(f)

def fmapTwice[F[_]: Functor, A](x: F[A])(f: A => A): F[A] =
  x.fmap(f).fmap(f)

def functor(x: Int ? String, y: Int?) =
  val _: Int ? String = fmapTwice(x)(_ + 1)
  val _: Int? = fmapTwice(y)(_ + 1)
  val _: String ? String = x.fmap(_.toString)

trait Monad[F[_]] extends Functor[F]:
  def pure[A](x: A): F[A]
  extension [A](x: F[A]) def bind[B](f: A => F[B]): F[B]
  extension [A](x: F[A]) def fmap[B](f: A => B): F[B] = x.bind(a => pure(f(a)))

given maybeMonad: [E] => Monad[[X] =>> X ? E]:
  def pure[A](x: A): A ? E = Ok(x)
  extension [A](x: A ? E) def bind[B](f: A => B ? E): B ? E = x.flatMap(f)

def sequence[F[_], A](xs: List[F[A]])(using m: Monad[F]): F[List[A]] =
  xs.foldRight(m.pure(Nil: List[A])): (x, acc) =>
    x.bind(a => acc.bind(as => m.pure(a :: as)))

def sequenced(xs: List[Int ? String]) =
  val _: List[Int] ? String = sequence(xs)

// An alias for a maybe type with a fixed error type
type Result[+T] = T ? String

def alias(x: Result[Int]) =
  val _: Result[Boolean] = foo(x)
  val _: Boolean ? String = foo(x)

// Explicit type arguments can still abstract over the error type
def explicitRight(x: String ? Int) =
  val _: String ? Boolean = foo[[X] =>> String ? X](x)

// A two-parameter type constructor matches `Maybe` directly
def foo2[F[_, _]](x: F[Int, String]): F[Boolean, String] = ???

def twoParams(x: Int ? String) =
  val _: Boolean ? String = foo2(x)

// Other type constructors keep right bias
def either(x: Either[String, Int]) =
  val _: Either[String, Boolean] = foo(x)

def map(x: Map[String, Int]) =
  val _: Map[String, Boolean] = foo(x)
