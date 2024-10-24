case class pc(calls: Any*) extends annotation.TypeConstraint

object Main {
  class C0 { def baz: String = "" }
  class C1 { def bar(c0: C0): String @pc(c0.baz) = c0.baz }
}
