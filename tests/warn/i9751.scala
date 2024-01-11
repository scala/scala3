

def f(): Unit = {
  () // warn
  ()
}

inline def g(): Unit = {
  () // warn
  ()
}