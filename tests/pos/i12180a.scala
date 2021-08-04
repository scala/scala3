inline def n = null
inline def f = inline if n == null then 0 else 1
inline def m = inline n match { case null => 0; case _ => 1 }

@main def main =
  println(f) // error
  println(m)
