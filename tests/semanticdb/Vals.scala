package example

abstract class Vals(p: Int, val xp: Int, var yp: Int) {
  val xm: Int = ???
  val xam: Int
  private[this] val xlm: Int = ???
  lazy val xzm: Int = ???
  private[this] lazy val xzlm: Int = ???
  final val xfm: Int = ???
  implicit val xim: Int = ???
  var ym: Int = ???
  var yam: Int
  private[this] var ylm: Int = ???
  // NOTE: lazy not allowed here. Only vals can be lazy
  // lazy var xzm: Int = ???
  // private[this] lazy var yzlm: Int = ???
  final var yfm: Int = ???
  implicit var yim: Int = ???
  def m = {
    val xl: Int = ???
    lazy val xzl: Int = ???
    // NOTE: local values cannot be final
    // final val xfl: Int = ???
    implicit val xil: Int = ???
    var yl: Int = ???
    // NOTE: lazy not allowed here. Only vals can be lazy
    // lazy var yzl: Int = ???
    // NOTE: local variables cannot be final
    // final var yfl: Int = ???
    implicit var yil: Int = ???
    yl = xl + yl
    println(xzl)
    yil = xil + yil
  }
  println(xzlm)
  ylm = xlm + ylm
}

object ValUsages {
  val v: Vals = ???
  v.yp = v.xp + v.yp
  v.ym = v.xm + v.ym
  v.yam = v.xam + v.yam
  println(v.xzm)
  v.yfm = v.xfm + v.yfm
  v.yim = v.xim + v.yim
}
