//> using options -Yno-flexible-types

import both.B

def b1(b: B): String = b.get() // error
def b2(b: B): String = b.markedGet()
