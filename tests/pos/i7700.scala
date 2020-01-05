package test

trait Show[-A] with
  def show(a: A): String

object Macros with
  inline def (sc: StringContext) show(args: =>Any*): String = ???

object Show with
  def[A] (a: A) show(given S: Show[A]): String = S.show(a)

  export Macros.show