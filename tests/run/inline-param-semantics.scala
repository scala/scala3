
inline def f1(x: Int): Int =
  println(s"f1($x)")
  x

inline def f2(inline x: Int): Int =
  println(s"f2($x)")
  x

inline def g1(x: =>Int): Int =
  println(s"g1($x)")
  x

inline def g2(inline x: =>Int): Int =
  println(s"g2($x)")
  x

inline def h1(x: Int*): Int =
  println(s"h1($x)")
  x.sum

inline def h2(inline x: Int*): Int =
  println(s"h2($x)")
  x.sum

inline def i1(x: =>Int*): Int =
  println(s"i1($x)")
  x.sum

inline def i2(inline x: =>Int*): Int =
  println(s"i2($x)")
  x.sum

def y: Int =
  println("y")
  43

@main() def Test: Unit = {
  f1(42); println()
  f2(42); println()
  g1(42); println()
  g2(42); println()
  h1(42); println()
  h2(42); println()
  i1(42); println()
  i2(42); println()
  f1(y); println()
  f2(y); println()
  g1(y); println()
  g2(y); println()
  h1(y, y); println()
  h2(y, y); println()
  i1(y, y); println()
  i2(y, y)
}
