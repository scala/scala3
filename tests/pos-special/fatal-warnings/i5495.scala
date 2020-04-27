class A
class B

type AorB = A | B

def foo(any: Any) = any match {
  case aorb: AorB =>
  case _ =>
}

