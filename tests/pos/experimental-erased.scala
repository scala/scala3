import language.experimental.erasedDefinitions
import annotation.experimental

@experimental
erased class CanThrow[-E <: Exception](val i: Int = 0)

@experimental
object Foo

@experimental
def bar = 1
