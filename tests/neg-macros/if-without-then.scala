//> using options -Yexplicit-nulls
import language.experimental.errorHandling

def foo =
  if (true) println("yes") // error

  if (true)             // error
    println("yes")

  if true else "error"  // error


