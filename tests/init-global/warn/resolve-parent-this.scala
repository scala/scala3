class Delegate {
  def foo() = f
  val f: O.type = O
}

object O extends Delegate {
  val a: Int = foo().a // warn
}