import scala.quoted._

object Macros {

  inline def blackbox: Int = ${one}

  transparent inline def whitebox: Int = ${one}

  private def one(using Quotes): Expr[Int] = Expr(1)

}
