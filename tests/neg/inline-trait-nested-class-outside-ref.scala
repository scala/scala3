//> using options -language:experimental.inlineTraits
inline trait A:
    class InnerA: // error: Inline traits may not define inner classes or traits.
        val x = 10

class B extends A:
   def foo = 10

def x = 
    val b = B()
    val c = b.InnerA()
    c
