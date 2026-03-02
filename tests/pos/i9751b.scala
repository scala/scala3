//> using options -Werror -deprecation -feature

object Test {
  inline def f(inline x: Boolean): Unit =
    inline if x then println()

  f(true)
  f(false)

  inline def g(inline x: => Boolean): Unit =
    inline if x then println()

  g(true)
  g(false)
}
