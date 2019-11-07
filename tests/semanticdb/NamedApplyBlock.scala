package example

object NamedApplyBlockMethods {
  val local = 1
  def foo(a: Int = 1, b: Int = 2, c: Int = 3): Int = a + b + c
  def baseCase = foo(local, c = 3) // as named apply is desugared, it would take more work to inspect within the body of the setter
  def recursive = foo(local, c = foo(local, c = 3)) // as named apply is desugared, it would take more work to inspect within the body of the setter
}

object NamedApplyBlockCaseClassConstruction {
  case class Msg(body: String, head: String = "default", tail: String)
  val bodyText = "body"
  val msg = Msg(bodyText, tail = "tail")
}
