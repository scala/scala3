object Test {
  inline def f(inline x: Boolean): Unit =
    inline if x then println()

  f(true)
  f(false)
}