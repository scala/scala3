//> using options -experimental

import language.experimental.erasedDefinitions
import annotation.experimental

erased class Bar

erased def bar = 2

erased val bar2 = 2

def bar3(erased a: Int) = 2
