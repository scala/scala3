enum Enum:
  case Case

object Enum:
  object nested:              
    val a: Enum = Case

  val b: Enum = f(nested.a)   

  def f(e: Enum): Enum = e

@main def main(): Unit = println(Enum.b)

// nopos-error: No warnings can be incurred under -Werror.