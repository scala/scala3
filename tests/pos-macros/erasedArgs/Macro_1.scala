import scala.quoted._
import scala.language.experimental.erasedDefinitions

transparent inline def mcr: Any = ${ mcrImpl(1, 2d, "abc") }

def mcrImpl(x: Int, erased y: Double, z: String)(using Quotes): Expr[String] =
  Expr(x.toString() + z)
