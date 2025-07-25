object Outer:
  inline def outer(inline x: Any): Unit =
    Inner.pos(x)
