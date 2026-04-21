inline trait A:
    opaque type Special = Int
    inline val b = 10
    def x: Special = b

class B extends A

def foo = 
    val b = B()
    println(b.x)
 