object Test:
  def main(args: Array[String]): Unit =
    val s = bar.Sub()
    println(s.direct)
    println(s.inherited)
    println(s.lambda)
    println(s.anon)
    println(s.inner)
    println(s.viaInline) // inlined outside of the subclass, needs an accessor
    println(s.field)
    println(foo.SamePackage.poll)
