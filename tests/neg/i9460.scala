trait A(val s: String) { println(s) }
trait B extends A { override val s = "B" } // requires override val s
class C extends B  // error
@main def Test = C()
