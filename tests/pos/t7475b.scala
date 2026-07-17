trait U {
}

trait T {
  type TT = Any & T & U
  private val priv = 0
  (??? : TT).priv
}
