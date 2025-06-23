import language.experimental.erasedDefinitions
import annotation.experimental

@experimental
erased class Foo

erased class Bar

@experimental
erased val foo2 = 2

erased val bar2 = 2

@experimental
def foo3(erased a: Int) = 2

def bar3(erased a: Int) = 2
