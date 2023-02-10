import scala.quoted.*

object Main {
  def foo(a: Expr[Any])(using Quotes) = {
    a match {
      case '{ ($x: Set[t]).toSet } =>
      case '{ ($x: Set[t]).toSet[t] } =>
      case '{ val a = 1; a; ($x: Set[t]).toSet } =>
      case '{ type u; ($x: Set[`u`]).toSet } =>
      case '{ type t; ($x: Set[t]).toSet } =>
      case '{ varargs(${_}*) } =>
      case '{ type u; ($x: t, $y: t, $z: u) } =>
      case _ =>
    }
  }
}

def varargs(x: Any*): Unit = ()
