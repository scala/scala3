package test

trait Show[-A]:
  def show(a: A): String

object Macros:
  inline def (sc: StringContext).show(args: =>Any*): String = ???

object Show:
  def[A] (a: A) show with (S: Show[A]) : String = S.show(a)

  export Macros.show