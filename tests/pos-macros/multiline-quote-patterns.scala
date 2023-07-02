import scala.quoted.*
def types(t: Type[?])(using Quotes) = t match {
  case '[
    type t;
    t
  ] =>

  case '[
    type t
    t
  ] =>

  case '[
    type t
    List[t]
  ] =>

  case '[
    type t;
    type u;
    Map[t, u]
  ] =>

  case '[
    type t
    type u
    Map[t, u]
  ] =>

  case '[
    type t; type u
    t => u
  ] =>
}

def expressions(x: Expr[Any])(using Quotes) = x match {
  case '{
    type t;
    $x: t
  } =>

  case '{
    type t
    $x: t
  } =>

  case '{
    type t;
    List()
  } =>

  case '{
    type t
    List()
  } =>

  case '{
    type t
    type u
    Map.empty[t, u]
  } =>
}
