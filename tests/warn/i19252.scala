//> using options -Werror -Wunused:all
object Deps:
  trait D1
  object D2
end Deps

object Bug:
  import Deps.D1 // no warn

  class Cl(d1: D1):
    import Deps.*
    def f = (d1, D2)
end Bug
