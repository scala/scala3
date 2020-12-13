import scala.quoted._



object Macros {

  implicit inline def testMacro: Unit = ${impl}

  def impl(using Quotes): Expr[Unit] = {

    val buff = new StringBuilder
    def stagedPrintln(x: Any): Unit = buff append java.util.Objects.toString(x) append "\n"

    Expr(3) match { case Const(n) => stagedPrintln(n) }
    '{4} match { case Const(n) => stagedPrintln(n) }
    '{"abc"} match { case Const(n) => stagedPrintln(n) }
    '{null} match { case '{null} => stagedPrintln(null) }

    '{new Object} match { case Const(n) => println(n); case _ => stagedPrintln("OK") }

    '{print(${Expr(buff.result())})}
  }
}
