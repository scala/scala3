//> using options -deprecation

object Deprecated:

  @deprecated("test")
  def method() = ???

  @deprecated("test")
  inline def inlineMethod() = ???

object Test:
  Deprecated.method() // warn
  Deprecated.inlineMethod() // warn
