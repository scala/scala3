//> using options -preview
import Conversion.into

type Input = List[String]

trait ParseResult[+T]
case class Success[+T](result: T, rest: Input) extends ParseResult[T]
case class Failure(msg: String) extends ParseResult[Nothing]

class Parser[+T](val parse: Input => ParseResult[T])

def empty[T](x: T) = Parser(in => Success(x, in))
def fail(msg: String) = Parser(in => Failure(msg))

class ParserOps[T](p: Parser[T]):
  def ~ [U](q: => into[Parser[U]]): Parser[(T, U)] = Parser(in =>
      p.parse(in) match
        case Success(x, in1) =>
          q.parse(in1) match
            case Success(y, in2) => Success((x, y), in2)
            case fail: Failure => fail
        case fail: Failure => fail
    )
  def | [U](q: => into[Parser[T]]): Parser[T] = Parser(in =>
      p.parse(in) match
        case s: Success[_] => s
        case fail: Failure => q.parse(in)
    )
  def map[U](f: T => U): Parser[U] = Parser(in =>
      p.parse(in) match
        case Success(x, in1) => Success(f(x), in1)
        case fail: Failure => fail
    )
  def ~> [U](q: => into[Parser[U]]): Parser[U] =
    (p ~ q).map(_(1))
  def <~ [U](q: => into[Parser[U]]): Parser[T] =
    (p ~ q).map(_(0))
  def parseAll(in: Input): ParseResult[T] =
    p.parse(in) match
      case succ @ Success(x, in1) =>
        if in1.isEmpty then succ
        else Failure(
          s"""Could not parse all of input
             |parse result   : $x
             |remaining input: ${in1.mkString(" ")}""".stripMargin)
      case fail: Failure =>
        fail

given strToToken: Conversion[String, Parser[String]] = token(_)

extension [T](p: Parser[T])
  private def ops = new ParserOps(p)
  export ops.*

extension (str: String)
  private def ops = new ParserOps(token(str))
  export ops.*

def token(p: String => Boolean, expected: String): Parser[String] = Parser {
  case first :: rest =>
    if p(first) then Success(first, rest)
    else Failure(s"$expected expected but `$first` found")
  case _ => Failure(s"premature end of input where $expected was expected")
}

def token(str: String): Parser[String] = token(str == _, s"`$str`")

def opt[T](p: into[Parser[T]]): Parser[Option[T]] =
  p.map(Some(_)) | empty(None)

def rep[T](p: into[Parser[T]]): Parser[List[T]] =
    (p ~ rep(p)).map(_ :: _)
  | empty(Nil)

object `~~`:
  def unapply[A, B](x: (A, B)): Some[(A, B)] = Some(x)

def reduce(x: Double, ops: List[(String,  Double)]): Double = (ops: @unchecked) match
  case Nil => x
  case ("+", y) :: ys => reduce(x + y, ys)
  case ("-", y) :: ys => reduce(x - y, ys)
  case ("*", y) :: ys => reduce(x * y, ys)
  case ("/", y) :: ys => reduce(x / y, ys)

def Expr: Parser[Double] =
  (Term ~ rep("+" ~ Term | "-" ~ Term)).map(reduce)
def Term: Parser[Double] =
  (Factor ~ rep("*" ~ Factor | "/" ~ Factor)).map(reduce)
def Factor: Parser[Double] =
  Number | "(" ~> Expr <~ ")"
def Number: Parser[Double] =
  token(_.toDoubleOption.isDefined, "number").map(_.toDouble)

def ops: Parser[String] = "+" | "-" | "*" | "/"

@main def Test =
  println(Expr.parseAll("2 * ( 3 + 4 - 2 / 1 )".split(" ").toList))
  println(Expr.parseAll("2 * ( 3 + 4 - 2 / 1".split(" ").toList))
