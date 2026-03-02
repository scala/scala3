class Common:

  trait Ord[T]:
    extension (x: T) def compareTo(y: T): Int
    extension (x: T) def < (y: T) = x.compareTo(y) < 0
    extension (x: T) def > (y: T) = x.compareTo(y) > 0

  trait Convertible[From, To]:
    extension (x: From) def convert: To

  trait SemiGroup[T]:
    extension (x: T) def combine(y: T): T

  trait Monoid[T] extends SemiGroup[T]:
    def unit: T

  trait Functor[F[_]]:
    extension [A](x: F[A]) def map[B](f: A => B): F[B]

  trait Monad[F[_]] extends Functor[F]:
    extension [A](x: F[A]) def flatMap[B](f: A => F[B]): F[B]
    extension [A](x: F[A]) def map[B](f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
end Common

object Instances extends Common:

  given intOrd: Ord[Int]:
    extension (x: Int) def compareTo(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0

  given listOrd: [T] => Ord[T] => Ord[List[T]]:
    extension (xs: List[T]) def compareTo(ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
  end listOrd

  extension (xs: Seq[String])
    def longestStrings: Seq[String] =
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)

  extension [T](xs: List[T])
    def second = xs.tail.head
    def third = xs.tail.tail.head

  given listMonad: Monad[List]:
    extension [A](xs: List[A]) def flatMap[B](f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)

  given readerMonad: [Ctx] => Monad[[X] =>> Ctx => X]:
    extension [A](r: Ctx => A) def flatMap[B](f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x

  def maximum[T](xs: List[T])(using Ord[T]): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T](using asc: Ord[T]): Ord[T] = new Ord[T]:
    extension (x: T) def compareTo(y: T) = asc.compareTo(y)(x)

  def minimum[T](xs: List[T])(using Ord[T]) =
    maximum(xs)(using descending)

  def test(): Unit =
    val xs = List(1, 2, 3)
    println(maximum(xs))
    println(maximum(xs)(using descending))
    println(maximum(xs)(using descending(using intOrd)))
    println(minimum(xs))

  case class Context(value: String)
  val c0: Context ?=> String = ctx ?=> ctx.value
  val c1: Context ?=> String = (ctx: Context) ?=> ctx.value

  class A
  class B
  val ab: (x: A, y: B) ?=> Int = (a: A, b: B) ?=> 22

  trait TastyAPI:
    type Symbol
    trait SymDeco:
      extension (sym: Symbol) def name: String
    given symDeco: SymDeco

  object TastyImpl extends TastyAPI:
    type Symbol = String
    given symDeco: SymDeco:
      extension (sym: Symbol) def name = sym

  class D[T]

  class C(using ctx: Context):
    def f() =
      locally {
        given Context = this.ctx
        println(summon[Context].value)
      }
      locally {
        lazy val ctx1 = this.ctx
        given Context = ctx1
        println(summon[Context].value)
      }
      locally {
        given d[T]: D[T]()
        println(summon[D[Int]])
      }
      locally {
        given (using Context): D[Int]()
        println(summon[D[Int]])
      }
  end C

  class Token(str: String)

  object Token:
    given StringToToken: Conversion[String, Token]:
      def apply(str: String): Token = new Token(str)

  val x: Token = "if"
end Instances

object PostConditions:
  opaque type WrappedResult[T] = T

  def result[T](using x: WrappedResult[T]): T = x

  extension [T](x: T)
    def ensuring(condition: WrappedResult[T] ?=> Boolean): T =
      assert(condition(using x))
      x
end PostConditions

object AnonymousInstances extends Common:
  given Ord[Int]:
    extension (x: Int) def compareTo(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0

  given [T: Ord] => Ord[List[T]]:
    extension (xs: List[T]) def compareTo(ys: List[T]): Int = (xs, ys).match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)

  extension (xs: Seq[String])
    def longestStrings: Seq[String] =
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)

  extension [T](xs: List[T])
    def second = xs.tail.head

  given [From, To](using c: Convertible[From, To])
      : Convertible[List[From], List[To]] with
    extension (x: List[From]) def convert: List[To] = x.map(c.convert)

  given Monoid[String]:
    extension (x: String) def combine(y: String): String = x.concat(y)
    def unit: String = ""

  def sum[T: Monoid](xs: List[T]): T =
      xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))
end AnonymousInstances

object Implicits extends Common:
  implicit object IntOrd extends Ord[Int]:
    extension (x: Int) def compareTo(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0

  class ListOrd[T: Ord] extends Ord[List[T]]:
    extension (xs: List[T]) def compareTo(ys: List[T]): Int = (xs, ys).match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
  implicit def ListOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]

  class given_Convertible_List_List[From, To](implicit c: Convertible[From, To])
  extends Convertible[List[From], List[To]]:
    extension (x: List[From]) def convert: List[To] = x.map(c.convert)
  implicit def given_Convertible_List_List[From, To](implicit c: Convertible[From, To])
    : Convertible[List[From], List[To]] =
    new given_Convertible_List_List[From, To]

  def maximum[T](xs: List[T])
                (implicit cmp: Ord[T]): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T](implicit asc: Ord[T]): Ord[T] = new Ord[T]:
    extension (x: T) def compareTo(y: T) = asc.compareTo(y)(x)

  def minimum[T](xs: List[T])(implicit cmp: Ord[T]) =
    maximum(xs)(descending)

object Test extends App:
  Instances.test()
  import PostConditions.{result, ensuring}
  val s = List(1, 2, 3).sum
  s.ensuring(result == 6)
end Test

object Completions:

  class Future[T]
  class HttpResponse
  class StatusCode

  // The argument "magnet" type
  enum CompletionArg:
    case Error(s: String)
    case Response(f: Future[HttpResponse])
    case Status(code: Future[StatusCode])
  object CompletionArg:

    // conversions defining the possible arguments to pass to `complete`
    // these always come with CompletionArg
    // They can be invoked explicitly, e.g.
    //
    //   CompletionArg.from(statusCode)

    given fromString    : Conversion[String, CompletionArg] = Error(_)
    given fromFuture    : Conversion[Future[HttpResponse], CompletionArg] = Response(_)
    given fromStatusCode: Conversion[Future[StatusCode], CompletionArg] = Status(_)
  import CompletionArg.*

  def complete[T](arg: CompletionArg) = arg match
    case Error(s) => ???
    case Response(f) => ???
    case Status(code) => ???
end Completions
