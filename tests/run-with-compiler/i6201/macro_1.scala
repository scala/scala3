import scala.quoted._
import scala.tasty._

inline def (inline x: String) strip: String =
  ${ stripImpl(x) }

def stripImpl(x: String)(implicit refl: Reflection): Expr[String] =
  x.stripMargin.toExpr

inline def isHello(inline x: String): Boolean =
  ${ isHelloImpl(x) }

def isHelloImpl(x: String)(implicit refl: Reflection): Expr[Boolean] =
  if (x == "hello") true.toExpr else false.toExpr

