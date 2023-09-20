import language.experimental.erasedDefinitions
import annotation.experimental

@experimental
erased class Foo

@experimental
erased def foo = 2

@experimental
erased val foo2 = 2

@experimental
def foo3(erased a: Int) = 2
