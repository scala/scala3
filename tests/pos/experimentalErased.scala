import language.experimental.erasedDefinitions
import annotation.experimental

@experimental
erased class Foo

erased class Bar

@experimental
erased def foo: Int

erased def bar: Int

@experimental
erased val foo2: Int

erased val bar2: Int

@experimental
def foo3(erased a: Int) = 2

def bar3(erased a: Int) = 2
