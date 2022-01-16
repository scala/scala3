def f(): Unit = {
  () // error
  ()
}

inline def g(): Unit = {
  () // error
  ()
}

inline def h(inline x: => Boolean): Unit =
  if x then println()
  else x

