import caps.assumeSafe

object A:
  inline def f(x: Int) = x
  @assumeSafe inline def g(x: Any) = x.asInstanceOf[Int]
  transparent inline def f2(x: Int) = x
  @assumeSafe transparent inline def g2(x: Any) = x.asInstanceOf[Int]

