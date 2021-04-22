inline def n = "im a string"
inline def f = inline if n == null then 0 else 1
inline def g = inline if null == n then 0 else 1
inline def m = inline n match { case null => 0; case _ => 1 }

@main def main =
  println(f)
  println(g)
  println(m)
