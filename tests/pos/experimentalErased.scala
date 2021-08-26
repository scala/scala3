import language.experimental.erasedDefinitions
import annotation.experimental

@experimental
erased class Foo

erased class Bar

@experimental
erased def foo = 2

erased def bar = 2

@experimental
erased val foo2 = 2

erased val bar2 = 2

@experimental
def foo3(erased a: Int) = 2

def bar3(erased a: Int) = 2
