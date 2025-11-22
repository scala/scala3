//> using options -Werror

class A
class B

type AorB = A | B

def foo(any: Any) = any match {
  case aorb: AorB =>
  case _ =>
}

def bar[T](x: List[T]) = x.isInstanceof[List[Int]] // error

