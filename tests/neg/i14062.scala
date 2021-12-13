import annotation.*

object Test:

  @targetName("") // error
  def foo = println("ok")

  foo


