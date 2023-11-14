//> using options -Ycheck-all-patmat
def foo(x: Array[String]) = x match {
  case _: Array[?] =>
}

def bar(x: Array[String]) = x match {
  case _: Array[? <: Int] =>
}
