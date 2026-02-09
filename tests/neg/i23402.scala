//> using options -explain

class A(p1: String, p2: Int)
object A {
 def apply(p1: String, p2: Int): A = A(p1, p2)
 def apply(p1: String)(p2: Int): A = A(p1, p2) // error
}
