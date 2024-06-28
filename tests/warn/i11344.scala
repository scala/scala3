//> using options  -deprecation

trait Pet(val name: String, rest: Int):
   def f(suffix: String) = s"$name$suffix$rest"

class Birdie(override val name: String) extends Pet("huh", 1) // warn



