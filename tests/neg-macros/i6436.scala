import scala.quoted._

def f(s: Scope)(sc: s.Expr[StringContext]): Unit = {
  sc match {
    case '{ StringContext(${Varargs(parts)}: _*) } => // error
      val ps: Seq[s.Expr[String]] = parts // error
  }
}