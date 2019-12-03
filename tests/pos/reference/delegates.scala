class Common with

  trait Ord[T] with
    def (x: T) compareTo (y: T): Int
    def (x: T) < (y: T) = x.compareTo(y) < 0
    def (x: T) > (y: T) = x.compareTo(y) > 0

  trait Convertible[From, To] with
    def (x: From) convert: To

  trait SemiGroup[T] with
    def (x: T) combine (y: T): T

  trait Monoid[T] extends SemiGroup[T] with
    def unit: T

  trait Functor[F[_]] with
    def [A, B](x: F[A]) map (f: A => B): F[B]

  trait Monad[F[_]] extends Functor[F] with
    def [A, B](x: F[A]) flatMap (f: A => F[B]): F[B]
    def [A, B](x: F[A]) map (f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]

object Instances extends Common with

  given intOrd: Ord[Int] with
    def (x: Int) compareTo (y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0

  given listOrd[T](given Ord[T]): Ord[List[T]] with
    def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
  end listOrd

  given stringOps: (xs: Seq[String]) extended with
    def longestStrings: Seq[String] =
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)

  given [T](xs: List[T]) extended with
    def second = xs.tail.head
    def third = xs.tail.tail.head

  given listMonad: Monad[List] with
    def [A, B](xs: List[A]) flatMap (f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)

  given readerMonad[Ctx]: Monad[[X] =>> Ctx => X] with
    def [A, B](r: Ctx => A) flatMap (f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x

  def maximum[T](xs: List[T])(given Ord[T]): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T](given asc: Ord[T]): Ord[T] = new Ord[T] with
    def (x: T) compareTo (y: T) = asc.compareTo(y)(x)

  def minimum[T](xs: List[T])(given Ord[T]) =
    maximum(xs)(given descending)

  def test(): Unit =
    val xs = List(1, 2, 3)
    println(maximum(xs))
    println(maximum(xs)(given descending))
    println(maximum(xs)(given descending(given intOrd)))
    println(minimum(xs))

  case class Context(value: String)
  val c0: (given Context) => String = (given ctx) => ctx.value
  val c1: ((given Context) => String) = (given ctx: Context) => ctx.value

  class A
  class B
  val ab: (given x: A, y: B) => Int = (given a: A, b: B) => 22

  trait TastyAPI with
    type Symbol
    trait SymDeco with
      def (sym: Symbol) name: String
    def symDeco: SymDeco
    given SymDeco = symDeco

  object TastyImpl extends TastyAPI with
    type Symbol = String
    val symDeco = new SymDeco with
      def (sym: Symbol) name = sym

  class D[T]

  class C(given ctx: Context) with
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
        given d[T]: D[T]
        println(summon[D[Int]])
      }
      locally {
        given (given Context): D[Int]
        println(summon[D[Int]])
      }
  end C

  class Token(str: String)

  object Token with
    given StringToToken : Conversion[String, Token] with
      def apply(str: String): Token = new Token(str)

  val x: Token = "if"
end Instances

object PostConditions with
  opaque type WrappedResult[T] = T

  def result[T](given x: WrappedResult[T]): T = x

  given extension [T](x: T)
    def ensuring(condition: (given WrappedResult[T]) => Boolean): T =
      assert(condition(given x))
      x
end PostConditions

object AnonymousInstances extends Common with
  given Ord[Int] with
    def (x: Int) compareTo (y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0

  given [T: Ord] : Ord[List[T]] with
    def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)

  given extension (xs: Seq[String])
    def longestStrings: Seq[String] =
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)

  given extension [T](xs: List[T])
    def second = xs.tail.head

  given [From, To](given c: Convertible[From, To]) : Convertible[List[From], List[To]] with
    def (x: List[From]) convert: List[To] = x.map(c.convert)

  given Monoid[String] with
    def (x: String) combine (y: String): String = x.concat(y)
    def unit: String = ""

  def sum[T: Monoid](xs: List[T]): T =
      xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))
end AnonymousInstances

object Implicits extends Common with
  implicit object IntOrd extends Ord[Int] with
    def (x: Int) compareTo (y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0

  class ListOrd[T: Ord] extends Ord[List[T]] with
    def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
  implicit def ListOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]

  class given_Convertible_List_List[From, To](implicit c: Convertible[From, To])
  extends Convertible[List[From], List[To]] with
    def (x: List[From]) convert: List[To] = x.map(c.convert)
  implicit def given_Convertible_List_List[From, To](implicit c: Convertible[From, To])
    : Convertible[List[From], List[To]] =
    new given_Convertible_List_List[From, To]

  def maximum[T](xs: List[T])
                (implicit cmp: Ord[T]): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T](implicit asc: Ord[T]): Ord[T] = new Ord[T] with
    def (x: T) compareTo (y: T) = asc.compareTo(y)(x)

  def minimum[T](xs: List[T])(implicit cmp: Ord[T]) =
    maximum(xs)(descending)

object Test extends App with
  Instances.test()
  import PostConditions.result
  import PostConditions.given
  val s = List(1, 2, 3).sum
  s.ensuring(result == 6)
end Test

object Completions with

  class Future[T]
  class HttpResponse
  class StatusCode

  // The argument "magnet" type
  enum CompletionArg with
    case Error(s: String)
    case Response(f: Future[HttpResponse])
    case Status(code: Future[StatusCode])
  object CompletionArg with

    // conversions defining the possible arguments to pass to `complete`
    // these always come with CompletionArg
    // They can be invoked explicitly, e.g.
    //
    //   CompletionArg.from(statusCode)

    given fromString : Conversion[String, CompletionArg] = Error(_)
    given fromFuture : Conversion[Future[HttpResponse], CompletionArg] = Response(_)
    given fromStatusCode : Conversion[Future[StatusCode], CompletionArg] = Status(_)
  import CompletionArg._

  def complete[T](arg: CompletionArg) = arg match
    case Error(s) => ???
    case Response(f) => ???
    case Status(code) => ???
end Completions
