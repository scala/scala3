class Common {

  trait Ord[T] {
    def (x: T) compareTo (y: T): Int
    def (x: T) < (y: T) = x.compareTo(y) < 0
    def (x: T) > (y: T) = x.compareTo(y) > 0
  }

  trait Convertible[From, To] {
    def (x: From) convert: To
  }

  trait SemiGroup[T] {
    def (x: T) combine (y: T): T
  }

  trait Monoid[T] extends SemiGroup[T] {
    def unit: T
  }

  trait Functor[F[_]] {
    def (x: F[A]) map[A, B] (f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def (x: F[A]) flatMap[A, B] (f: A => F[B]): F[B]
    def (x: F[A]) map[A, B] (f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
  }
}

object Instances extends Common {

  delegate IntOrd for Ord[Int] {
    def (x: Int) compareTo (y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
  }

  delegate ListOrd[T] for Ord[List[T]] given Ord[T] {
    def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
  }

  delegate StringOps {
    def (xs: Seq[String]) longestStrings: Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }

  delegate ListOps {
    def (xs: List[T]) second[T] = xs.tail.head
  }

  delegate ListMonad for Monad[List] {
    def (xs: List[A]) flatMap[A, B] (f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }

  delegate ReaderMonad[Ctx] for Monad[[X] =>> Ctx => X] {
    def (r: Ctx => A) flatMap[A, B] (f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
  }

  def maximum[T](xs: List[T]) given Ord[T]: T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T] given (asc: Ord[T]): Ord[T] = new Ord[T] {
    def (x: T) compareTo (y: T) = asc.compareTo(y)(x)
  }

  def minimum[T](xs: List[T]) given Ord[T] =
    maximum(xs) given descending

  def test(): Unit = {
    val xs = List(1, 2, 3)
    println(maximum(xs))
    println(maximum(xs) given descending)
    println(maximum(xs) given (descending given IntOrd))
    println(minimum(xs))
  }

  case class Context(value: String)
  val c0: given Context => String = given ctx => ctx.value
  val c1: (given Context => String) = given (ctx: Context) => ctx.value

  class A
  class B
  val ab: given (x: A, y: B) => Int = given (a: A, b: B) => 22

  trait TastyAPI {
    type Symbol
    trait SymDeco {
      def (sym: Symbol) name: String
    }
    def symDeco: SymDeco
    delegate for SymDeco = symDeco
  }
  object TastyImpl extends TastyAPI {
    type Symbol = String
    val symDeco = new SymDeco {
      def (sym: Symbol) name = sym
    }
  }

  class D[T]

  class C given (ctx: Context) {
    def f() = {
      locally {
        delegate for Context = this.ctx
        println(the[Context].value)
      }
      locally {
        lazy val ctx1 = this.ctx
        delegate for Context = ctx1
        println(the[Context].value)
      }
      locally {
        delegate d[T] for D[T]
        println(the[D[Int]])
      }
      locally {
        delegate for D[Int] given Context
        println(the[D[Int]])
      }
    }
  }

  class Token(str: String)

  object Token {
    delegate StringToToken for Conversion[String, Token] {
      def apply(str: String): Token = new Token(str)
    }
  }

  val x: Token = "if"
}

object PostConditions {
  opaque type WrappedResult[T] = T

  private delegate WrappedResult {
    def apply[T](x: T): WrappedResult[T] = x
    def (x: WrappedResult[T]) unwrap[T]: T = x
  }

  def result[T] given (wrapped: WrappedResult[T]): T = wrapped.unwrap

  delegate {
    def (x: T) ensuring[T] (condition: given WrappedResult[T] => Boolean): T = {
      assert(condition given WrappedResult(x))
      x
    }
  }
}

object AnonymousInstances extends Common {
  delegate for Ord[Int] {
    def (x: Int) compareTo (y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
  }

  delegate [T: Ord] for Ord[List[T]] {
    def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
  }

  delegate {
    def (xs: Seq[String]) longestStrings: Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }

  delegate {
    def (xs: List[T]) second[T] = xs.tail.head
  }

  delegate [From, To] for Convertible[List[From], List[To]] given (c: Convertible[From, To]) {
    def (x: List[From]) convert: List[To] = x.map(c.convert)
  }

  delegate for Monoid[String] {
    def (x: String) combine (y: String): String = x.concat(y)
    def unit: String = ""
  }

  def sum[T: Monoid](xs: List[T]): T =
      xs.foldLeft(the[Monoid[T]].unit)(_.combine(_))
}

object Implicits extends Common {
  implicit object IntOrd extends Ord[Int] {
    def (x: Int) compareTo (y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
  }

  class ListOrd[T: Ord] extends Ord[List[T]] {
    def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
  }
  implicit def ListOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]

  class Convertible_List_List_instance[From, To](implicit c: Convertible[From, To])
  extends Convertible[List[From], List[To]] {
    def (x: List[From]) convert: List[To] = x.map(c.convert)
  }
  implicit def Convertible_List_List_instance[From, To](implicit c: Convertible[From, To])
    : Convertible[List[From], List[To]] =
    new Convertible_List_List_instance[From, To]

  def maximum[T](xs: List[T])
                (implicit cmp: Ord[T]): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T](implicit asc: Ord[T]): Ord[T] = new Ord[T] {
    def (x: T) compareTo (y: T) = asc.compareTo(y)(x)
  }

  def minimum[T](xs: List[T])(implicit cmp: Ord[T]) =
    maximum(xs)(descending)
}

object Test extends App {
  Instances.test()
  import PostConditions.result
  import delegate PostConditions._
  val s = List(1, 2, 3).sum
  s.ensuring(result == 6)
}

object Completions {

  class Future[T]
  class HttpResponse
  class StatusCode

  // The argument "magnet" type
  enum CompletionArg {
    case Error(s: String)
    case Response(f: Future[HttpResponse])
    case Status(code: Future[StatusCode])
  }
  object CompletionArg {

    // conversions defining the possible arguments to pass to `complete`
    // these always come with CompletionArg
    // They can be invoked explicitly, e.g.
    //
    //   CompletionArg.from(statusCode)

    delegate fromString for Conversion[String, CompletionArg] = Error(_)
    delegate fromFuture for Conversion[Future[HttpResponse], CompletionArg] = Response(_)
    delegate fromStatusCode for Conversion[Future[StatusCode], CompletionArg] = Status(_)
  }
  import CompletionArg._

  def complete[T](arg: CompletionArg) = arg match {
    case Error(s) => ???
    case Response(f) => ???
    case Status(code) => ???
  }
}