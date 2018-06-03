object Test {

  import p.transparents._

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
  }

}
