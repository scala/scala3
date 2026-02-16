val u = Unit // error

inline def a(inline f: Unit ?=> Unit): Unit = f(using ())

def s5bug = a:
  print(2)
  Unit // error
