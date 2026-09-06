//> using options -Yexplicit-nulls -Werror
def f(x: String | Null) = x match
  case s: String => ()
  case _ => ()
