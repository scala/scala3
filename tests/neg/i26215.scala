//> using options -Ycheck:all

object O1:
    inline def x = 10

object O2:
    inline val y = O1.x // error: inline value must have a literal constant type
