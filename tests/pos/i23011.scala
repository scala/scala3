
type X = Double => Unit
def g(x: Double): Unit = ???
def g(x: String): Unit = ???

def f1(x: X) = ???

def f2(x: X, y: Int) = ???
def f2(x: X, y: Boolean) = ???

def f3(x: X)(y: Int) = ???
def f3(x: X)(y: Boolean) = ???

val r1 = f1(g) // ok
val r2 = f2(g, 1) // ok
val r3 = f3(g)(1) // was error: ambiguous overload for g
