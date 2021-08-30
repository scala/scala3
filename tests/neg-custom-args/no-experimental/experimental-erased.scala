import language.experimental.erasedDefinitions // error
import annotation.experimental

@experimental
erased class CanThrow[-E <: Exception]

def other = 1
