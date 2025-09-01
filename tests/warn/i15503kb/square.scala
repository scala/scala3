//> using options -Werror -Wunused:all

object PowerUser:
  import Power.*
  def square(x: Double): Double = powerMacro(x, 2)
