//> using options -opt -opt-inline:** -Yopt-inline-heuristics:everything

// Inlined code from the classpath puts values of types on the stack that codegen never created a
// ClassBType for (B, D). Computing the stack map frame at the merge points needs their LUB with C.
class C {
  def viaMethod(a: A): AnyRef = {
    // a.a is inlined, resulting in a.b, which has return type B
    var foo: AnyRef = if (hashCode == 0) a.a else this
    if (foo == null)
      foo = this
    foo
  }

  def viaField(): AnyRef = {
    // J_1.viaField is inlined, resulting in a read of J_1.f, which has type D
    var foo: AnyRef = if (hashCode == 0) J_1.viaField() else this
    if (foo == null)
      foo = this
    foo
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    assert(c.viaMethod(new A) eq c)
    assert(c.viaField() eq c)
  }
}
