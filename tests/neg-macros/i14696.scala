import scala.quoted.*

def test(a: Expr[Any])(using Quotes): Unit = a match
  case '{ ${_}: String } | '{ ${_}: Int } =>
  case '{ $x1: String } | '{ ${_}: Int } => // error
  case '{ $x: String } | '{ $x: Int } => // error // error
  case '{ $x1: String } | '{ ${x2: Expr[Int]} } => // error // error
  case '{ ${Str(x)}: String } | '{ ${y @ Str(z)}: Int } => // error // error // error
  case '{ val x: Int = ???; ${_}(x): String } | '{ '{ val x: String = ???; ${_}(x): String }} =>
  case '{ val x: Int = ???; $f(x): String } | '{ val x: String = ???; ${_}(x): String } => // error
  case '{ val x: Int = ???; $f(x): String } | '{ val x: String = ???; $f(x): String } => // error // error
  case '{ val x: Int = ???; $f(x): String } | '{ val x: String = ???; $g(x): String } => // error // error
  case '{ varargs(${_}*) } | '{  varargs(${_}*) } =>
  case '{ varargs($args*) } | '{  varargs(${_}*) } => // error
  case '{ varargs($args*) } | '{  varargs($args*) } => // error // error
  case '{ varargs($args1*) } | '{  varargs($args2*) } => // error // error
  case '{ ${_}: t } | '{ ${_}: Int } => // error
  case '{ ${_}: t } | '{ ${_}: t } => // error // error
  case '{ ${_}: t } | '{ ${_}: u } => // error // error
  case '{ type t; () } | '{ 1 } => // error
  case '{ type t; () } | '{ type t; () } => // error // error
  case '{ type t; () } | '{ type u; () } => // error // error

def varargs(x: Any*): Unit = ()

object Str:
  def unapply(x: Any): Option[String] = ???
