//> using options -Wsafe-init
import scala.quoted.*

class Macro:
  def tuple[T](name: String): (String, List[T]) = (name, List[T]())
  inline def nameTuple[T]: (String, List[T]) = tuple(Macro.named)

object Macro:
  def namedMacro(using q: Quotes): Expr[String] =
    Expr("test")

  inline def named: String = ${Macro.namedMacro}
