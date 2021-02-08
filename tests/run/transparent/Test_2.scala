object Test {

  import p.transparents.*

  def main(args: Array[String]): Unit = {
    println(f(10))
    println(f(f(10)))

    track("hello") { println("") }

    val o = new Outer
    val i = new o.Inner
    println(i.m)
    println(i.g)
    println(i.h)
    println(o.inner.m)
    println(o.inner.g)
    println(o.inner.h)

    val p = new TestPassing

    println(p.foo("hi"))
    println(p.bar(true))

  }
}
