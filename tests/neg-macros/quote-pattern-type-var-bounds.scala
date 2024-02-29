import scala.quoted.*
def types(t: Type[?])(using Quotes) = t match {
  case '[ type t; Int ] =>
  case '[ type t <: Int; Int ] =>
  case '[ type t >: 1 <: Int; Int ] =>
  case '[ type t = Int; Int ] => // error
  case '[ type t = scala.Int; Int ] => // error
  case '[ type f[t] <: List[Any]; Int ] =>
  case '[ type f[t <: Int] <: List[Any]; Int ] =>
  case '[ type f[t] = List[Any]; Int ] => // error
}

def expressions(x: Expr[Any])(using Quotes) = x match {
  case '{ type t; () } =>
  case '{ type t <: Int; () } =>
  case '{ type t >: 1 <: Int; () } =>
  case '{ type t = Int; () } =>  // error
  case '{ type t = scala.Int; () } =>  // error
  case '{ type f[t] <: List[Any]; () } =>
  case '{ type f[t <: Int] <: List[Any]; () } =>
  case '{ type f[t] = List[Any]; () } =>  // error
}
