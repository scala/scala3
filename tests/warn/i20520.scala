
//> using options -Wunused:all

@main def run =
  val veryUnusedVariable: Int = value // warn local

package i20520:
  private def veryUnusedMethod(x: Int): Unit = println() // warn param
  private val veryUnusedVariableToplevel: Unit = println() // package members are accessible under separate compilation

def value = 42
