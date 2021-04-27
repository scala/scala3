case class A(a: Int)
object C { def unapply(a: A): true = true }

@main
def Test =  (A(1): A | A) match { case C() => "OK" }