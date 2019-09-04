import scala.quoted._
import scala.quoted.matching._

object Macros {

  inline def (self: => StringContext) xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]]) given QuoteContext: Expr[String] = {
    self match {
      case '{ StringContext($parts: _*) } =>
        '{
          val p: Seq[String] = $parts
          val a: Seq[Any] = $args ++ Seq("")
          p.zip(a).map(_ + _.toString).mkString
        }
      case _ =>
        '{ "ERROR" }
    }
  }
}
