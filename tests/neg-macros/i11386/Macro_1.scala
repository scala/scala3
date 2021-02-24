import scala.language.implicitConversions
import scala.quoted._

object test {

  inline implicit def int2String(inline i: Int): String = {
    notNull(i)
    String.valueOf(i)
  }

  inline def notNull(inline i: Int): Unit = ${notNullImpl('i)}

  def notNullImpl(expr: Expr[Int])(using quotes: Quotes): Expr[Unit] = {
    expr.show
    expr.value.foreach(i => if(i == 0) quotes.reflect.report.error("test"))
    '{()}
  }
}
