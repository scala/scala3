class Foo[A] {
  // Previously, we did not record the constraint ordering `A <: B` here
  def bar[B >: A <: String]: String = ""
}
object Test {
  // ... which prevented the following from typechecking.
  val ret = (new Foo).bar
}
