// scalac: -Ycheck-all-patmat
object Test {
  def foo(x : AnyVal) = x match {case b : Boolean => "It's a bool"}
}
