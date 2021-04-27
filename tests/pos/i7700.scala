package test

trait Show[-A]:
  def show(a: A): String

object Macros:
  extension (sc: StringContext) inline def show(args: =>Any*): String = ???

object Show:
  extension [A] (a: A) def show(using S: Show[A]): String = S.show(a)

  export Macros.show