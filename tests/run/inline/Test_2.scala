object Test {

  import p.inlines._

  def main(args: Array[String]): Unit = {
    println(f(10))
    println(f(f(10)))

    track("hello") { println("") }

    val o = new Outer
    val i = new o.Inner
    val p = new TestPassing
    println(i.m)
    println(i.g)
    println(i.h)
    println(o.inner.m)
    println(o.inner.g)
    println(o.inner.h)
    println(p.foo("hi"))
    println(p.bar(true))
  }
}