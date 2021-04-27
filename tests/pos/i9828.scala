


extension (x: Int) inline def g (y: Int): Int = x + y
extension [S](f: S => Int) inline def f(s: String) = "test"
val z = 1.g(2)
def testStuff =  ((s: String) => 42).f("foo")

