trait U {
  private val priv = 0
  type TT = U & T // should allow `priv`
  (??? : TT).priv
}

trait Base {

}

trait T extends Base {

}
