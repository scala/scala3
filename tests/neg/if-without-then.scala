//> using options -Yexplicit-nulls
import language.experimental.magic

def foo =
  if (true) println("yes") // error

  if (true)             // error
    println("yes")

  if true else "error"  // error


