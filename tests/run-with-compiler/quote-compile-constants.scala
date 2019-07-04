import scala.quoted._

object Test {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {
    val qctx = the[QuoteContext]
    def check[T](expr: Expr[T]): Unit = {
      import qctx.tasty._
      println(expr.unseal)
    }

    check('{null})
    check('{true})
    check('{ 'a' })
    check('{ '\n' })
    check('{ '"' })
    check('{ '\'' })
    check('{ '\\' })
    check('{1})
    check('{ { { 2 } } })
    check('{3L})
    check('{4.0f})
    check('{5.0d})
    check('{"xyz"})
    check('{})
    check('{()})
    check('{{()}})
  }
}
