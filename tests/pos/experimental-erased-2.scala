import language.experimental.erasedDefinitions
import annotation.experimental

@experimental object Test:

  erased class CanThrow[-E <: Exception]

  def other = 1
