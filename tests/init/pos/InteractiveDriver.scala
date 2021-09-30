trait InteractiveDriver[A <: AnyVal] {
    val x: AnyVal
    def g(x: A): A
}

class C[A <: AnyVal] extends InteractiveDriver[A] {
    val x = 0
    def g(x: A) = x
}

class D[A <: AnyVal] extends InteractiveDriver[A] {
    val x = 1.5
    def g(x: A) = x
}

object InteractiveDriver {
    def h(x: InteractiveDriver[?]): C[?] = x match {
        case c: C[?] => c
        case _ => new C[Int]
    }
    val l: Seq[Any] = Seq(1, 2, new C[Double], new D[Int])
    val l2: Seq[C[?]] = l.collect{ case x: InteractiveDriver[?] => h(x) }
}
