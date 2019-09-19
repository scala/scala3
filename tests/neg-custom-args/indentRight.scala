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

  if (true)   // OK
    println("hi")
  }

  println("!")  // error: expected a toplevel definition
}
