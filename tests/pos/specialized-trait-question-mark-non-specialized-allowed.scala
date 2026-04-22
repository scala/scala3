//> using options -language:experimental.specializedTraits
inline trait T3[T: Specialized, E, F: Numeric]
def foo(v: T3[Int, ?, ?]) =
    println("HELLO")

def main = 
    val x = new T3[Int, String, Float] {}
    foo(x)
