trait A(s: String) { println(s) }
trait B extends A { val s = "B" }
class C extends B  // error
@main def Test = C()
