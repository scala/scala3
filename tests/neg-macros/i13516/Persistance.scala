import scala.quoted.*
object Persistance:
  inline def nameOf(inline e: Any): String = ${ nameOfImpl('e) }
  private def nameOfImpl(e: Expr[Any])(using Quotes): Expr[String] = Expr("")
  def foo(p: Versioned): Unit = {}
