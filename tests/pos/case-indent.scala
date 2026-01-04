import language.experimental.relaxedColonSyntax
import language.experimental.relaxedLambdaSyntax

def f[A](xs: List[A]): List[String] =
  xs.map:
  case s: String => s
  case x => x.toString
  .map:
  case s: String if s.length > 4 => s
  case s => f"$s%.04s"

def g(xs: List[Int]): List[String] =
  xs.map: case i: Int => i.toString // "relaxed lambda"
  .map:
  case s: String if s.length > 4 => s
  case s => f"$s%.04s"

class Extra:
  val pf: PartialFunction[String, Int] =
  case "foo" => 1
  case "bar" => 2

  def tryit(xs: List[String]) = xs.collect(pf)

class Possibly(b: Boolean):
  val pf: PartialFunction[String, Int] =
    if b then
    case "foo" => 1
    case "bar" => 2
    else
    case "foo" => 42
    case "bar" => 27

  def tryit(xs: List[String]) = xs.collect(pf)

class Functional:
  val f: Int => PartialFunction[String, Int] =
    i =>
    case _ => i

@main def main =
  println:
    f(List(42))
  println:
    g(List(42))
  println:
    Extra().tryit("baz" :: "bar" :: "foo" :: Nil)
  println:
    Possibly(false).tryit("baz" :: "bar" :: "foo" :: Nil)
