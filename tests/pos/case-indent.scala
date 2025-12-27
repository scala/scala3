import language.experimental.relaxedColonSyntax

def f[A](xs: List[A]): List[String] =
  xs.map:
  case s: String => s
  case x => x.toString

class Extra:
  val pf: PartialFunction[String, Int] =
  case "foo" => 1
  case "bar" => 2

  def tryit(xs: List[String]) = xs.collect(pf)

class Functional:
  val f: Int => PartialFunction[String, Int] =
    i =>
    case _ => i

@main def main =
  println:
    f(List(42))
  println:
    Extra().tryit("baz" :: "bar" :: "foo" :: Nil)
