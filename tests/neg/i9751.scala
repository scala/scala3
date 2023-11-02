//> using options -Xfatal-warnings

def f(): Unit = {
  () // warn
  ()
}

inline def g(): Unit = {
  () // warn
  ()
}
// nopos-error: No warnings can be incurred under -Werror.