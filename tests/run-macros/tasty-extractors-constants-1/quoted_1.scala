import scala.quoted.*



object Macros {

  implicit inline def testMacro: Unit = ${impl}

  def impl(using Quotes): Expr[Unit] = {

    val buff = new StringBuilder
    def stagedPrintln(x: Any): Unit = buff append java.util.Objects.toString(x) append "\n"

    Expr(3) match { case Expr(n) => stagedPrintln(n) }
    '{4} match { case Expr(n) => stagedPrintln(n) }
    '{"abc"} match { case Expr(n) => stagedPrintln(n) }
    '{null} match { case '{null} => stagedPrintln(null) }

    '{print(${Expr(buff.result())})}
  }
}
