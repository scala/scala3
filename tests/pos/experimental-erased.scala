import language.experimental.erasedDefinitions
import annotation.experimental

@experimental
class CanThrow[-E <: Exception](val i: Int = 0) extends compiletime.Erased

@experimental
object Foo

@experimental
def bar = 1
