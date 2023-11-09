abstract class Reader[+T] {
  def first: T

  def rest: Reader[T]

  def atEnd: Boolean
}

trait Parsers {
  type Elem
  type Input = Reader[Elem]

  sealed abstract class ParseResult[+T] {
    val successful: Boolean

    def map[U](f: T => U): ParseResult[U]

    def flatMapWithNext[U](f: T => Input => ParseResult[U]): ParseResult[U]
  }

  sealed abstract class NoSuccess(val msg: String) extends ParseResult[Nothing] { // when we don't care about the difference between Failure and Error
    val successful = false

    def map[U](f: Nothing => U) = this

    def flatMapWithNext[U](f: Nothing => Input => ParseResult[U]): ParseResult[U]
      = this
  }

  case class Failure(override val msg: String) extends NoSuccess(msg)

  case class Error(override val msg: String) extends NoSuccess(msg)

  case class Success[+T](result: T, val next: Input) extends ParseResult[T] {
    val successful = true

    def map[U](f: T => U) = Success(f(result), next)

    def flatMapWithNext[U](f: T => Input => ParseResult[U]): ParseResult[U] = f(result)(next) match {
      case s @ Success(result, rest) => Success(result, rest)
      case f: Failure => f
      case e: Error => e
    }
  }

  case class ~[+a, +b](_1: a, _2: b) {
    override def toString = s"(${_1}~${_2})"
  }

  abstract class Parser[+T] extends (Input => ParseResult[T]) {
    def apply(in: Input): ParseResult[T]

    def ~ [U](q: => Parser[U]): Parser[~[T, U]] = {
      (for(a <- this; b <- q) yield new ~(a,b))
    }

    def flatMap[U](f: T => Parser[U]): Parser[U]
      = Parser{ in => this(in) flatMapWithNext(f)}

    def map[U](f: T => U): Parser[U] //= flatMap{x => success(f(x))}
      = Parser{ in => this(in) map(f)}

    def ^^ [U](f: T => U): Parser[U] = map(f)
  }

  def Parser[T](f: Input => ParseResult[T]): Parser[T]
    = new Parser[T]{ def apply(in: Input) = f(in) }

  def accept(e: Elem): Parser[Elem] = acceptIf(_ == e)("'"+e+"' expected but " + _ + " found")

  def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = Parser { in =>
    if (in.atEnd) Failure("end of input")
    else if (p(in.first)) Success(in.first, in.rest)
    else Failure(err(in.first))
  }
}


object grammars3 extends Parsers {
  type Elem = String

  val a: Parser[String] = accept("a")
  val b: Parser[String] = accept("b")

  val AnBnCn: Parser[List[String]] = {
    repMany(a,b)
  }

  def repMany[T](p: => Parser[T], q: => Parser[T]): Parser[List[T]] =
    p~repMany(p,q)~q ^^ {case x~xs~y => x::xs:::(y::Nil)}
}