import language.experimental.erasedDefinitions
import annotation.experimental

@experimental
erased class CanThrow[-E <: Exception]

erased class CanThrow2[-E <: Exception] // error

def other = 1
