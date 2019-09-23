object Test {

  if (true)
    println("ho")
    println("hu") // error: Line is indented too far to the right

  if (true)
    { println("ho")
    }
    println("hu") // error: Line is indented too far to the right

  while (true)
  ()

  for (x <- List())  // OK
  {

  }

  trait A
    case class B() extends A  // error: Line is indented too far to the right
    case object C extends A   // error: Line is indented too far to the right

  if (true)   // OK
    println("hi")
  }

  println("!")  // error: expected a toplevel definition
}
