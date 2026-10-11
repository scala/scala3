trait T {
  type TT = T & Any
  private val priv = 0
  (??? : TT).priv
}

trait U {
  type UU = Any & U
  private val priv = 0
  (??? : UU).priv
}
