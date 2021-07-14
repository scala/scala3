// scalac: -Ycheck-all-patmat
def foo(x: Array[String]) = x match {
  case _: Array[_] =>
}

def bar(x: Array[String]) = x match {
  case _: Array[_ <: Int] =>
}
