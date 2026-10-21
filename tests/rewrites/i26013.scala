// https://github.com/scala/scala3/issues/26013
trait A
trait B
trait C
case class N(child: Any)

def m(n: N): String = n match
  case N(child: A with B) => "ab"
  case N(child: A with B with C) => "abc"
  case _: A with B => "wildcard"
  case N(child: (A with B)) => "already parens"
  case child: A with B { type T } => "refinement"
  case _ => "other"

def g(x: Any): String = x match
  case given A with B => "given ab"
  case _ => "other"

def h: Int with String = ???
