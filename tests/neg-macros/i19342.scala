import scala.quoted.*

def scrutinizeHoas(expr: Expr[Int])(using Quotes): Unit =
  expr match {
    case '{ ((y: Int) => $f(y)).apply($z: Int) } => () // error
    case _ => ()
  }
