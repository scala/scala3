//> using options -no-indent -Xfatal-warnings

trait A
    case class B() extends A  // warn: Line is indented too far to the right
    case object C extends A   // warn: Line is indented too far to the right

object Test {

  if (true)
    println("ho")
    println("hu") // warn: Line is indented too far to the right

  if (true)
    { println("ho")
    }
    println("hu") // warn: Line is indented too far to the right

  while (true)
  ()

  for (x <- List())  // OK
  {

  }

  trait A
    case class B() extends A  // warn: Line is indented too far to the right
    case object C extends A   // warn: Line is indented too far to the right

  if (true)   // OK
    println("hi")
  }

  println("!")  // error: expected a toplevel definition
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)