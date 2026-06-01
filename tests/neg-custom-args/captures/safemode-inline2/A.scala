import caps.assumeSafe

object A:
  @assumeSafe inline def g(x: Any) = x
  @assumeSafe transparent inline def g2(x: Any) = x

