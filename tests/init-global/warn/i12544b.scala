enum Enum:
  case Case

object Enum:
  object nested: // warn
    val a: Enum = Case

  val b: Enum = f(nested.a) // warn

  def f(e: Enum): Enum = e

@main def main(): Unit = println(Enum.b)
