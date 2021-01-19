package test

trait Show[-A] with
  def show(a: A): String

object Macros with
  extension (sc: StringContext) inline def show(args: =>Any*): String = ???

object Show with
  extension [A] (a: A) def show(using S: Show[A]): String = S.show(a)

  export Macros.show