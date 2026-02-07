import scala.quoted.*

class Macro:
  inline def nameTuple[NameTuple_T]: (String, List[NameTuple_T]) = Macro.tuple[NameTuple_T](Macro.named)

object Macro:
  def namedMacro(using q: Quotes): Expr[String] = Expr("test")
  inline def named: String = ${Macro.namedMacro}
  def tuple[Tuple_T](name: String): (String, List[Tuple_T]) = (name, List.empty[Tuple_T])
