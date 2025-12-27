import language.experimental.relaxedColonSyntax

def f[A](xs: List[A]): List[String] =
  xs.map:
  case s: String => s
  case x => x.toString

@main def main =
  println:
    f(List(42))
