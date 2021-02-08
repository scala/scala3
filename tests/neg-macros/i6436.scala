import scala.quoted.*

def f(sc: quoted.Expr[StringContext]): Unit = {
  sc match {
    case '{ StringContext(${Varargs(parts)}*) } => // error
      val ps: Seq[Expr[String]] = parts // error
  }
}