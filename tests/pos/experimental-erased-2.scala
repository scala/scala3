import language.experimental.erasedDefinitions
import annotation.experimental

@experimental object Test:

  class CanThrow[-E <: Exception] extends compiletime.Erased

  def other = 1
