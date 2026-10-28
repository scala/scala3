import scala.quoted.*

inline def memberNames[T]: (List[String], List[String]) = ${ memberNamesImpl[T] }

def memberNamesImpl[T: Type](using Quotes): Expr[(List[String], List[String])] =
  import quotes.reflect.*
  val sym = TypeRepr.of[T].typeSymbol
  val methods = sym.methodMembers.filter(_.owner == sym).map(_.name)
  val fields = sym.fieldMembers.filter(_.owner == sym).map(_.name)
  Expr((methods, fields))
