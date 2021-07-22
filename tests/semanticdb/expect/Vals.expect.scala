package example

abstract class Vals/*<-example::Vals#*/(p/*<-example::Vals#p.*/: Int/*->scala::Int#*/, val xp/*<-example::Vals#xp.*/: Int/*->scala::Int#*/, var yp/*<-example::Vals#yp().*/: Int/*->scala::Int#*/) {
  val xm/*<-example::Vals#xm.*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  val xam/*<-example::Vals#xam.*/: Int/*->scala::Int#*/
  private[this] val xlm/*<-example::Vals#xlm.*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  lazy val xzm/*<-example::Vals#xzm.*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  private[this] lazy val xzlm/*<-example::Vals#xzlm.*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  final val xfm/*<-example::Vals#xfm.*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  implicit val xim/*<-example::Vals#xim.*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  var ym/*<-example::Vals#ym().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  var yam/*<-example::Vals#yam().*/: Int/*->scala::Int#*/
  private[this] var ylm/*<-example::Vals#ylm().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  private[this] var _explicitSetter/*<-example::Vals#_explicitSetter().*/: Int/*->scala::Int#*/ = 0
  def explicitSetter/*<-example::Vals#explicitSetter().*/ = _explicitSetter/*->example::Vals#_explicitSetter().*/
  def explicitSetter_=/*<-example::Vals#`explicitSetter_=`().*/(x/*<-example::Vals#`explicitSetter_=`().(x)*/: Int/*->scala::Int#*/): Unit/*->scala::Unit#*/ = _explicitSetter/*->example::Vals#_explicitSetter().*/ = x/*->example::Vals#`explicitSetter_=`().(x)*/
  // NOTE: lazy not allowed here. Only vals can be lazy
  // lazy var xzm: Int = ???
  // private[this] lazy var yzlm: Int = ???
  final var yfm/*<-example::Vals#yfm().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  implicit var yim/*<-example::Vals#yim().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
  def m/*<-example::Vals#m().*/ = {
    val xl/*<-local0*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
    lazy val xzl/*<-local1*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
    // NOTE: local values cannot be final
    // final val xfl: Int = ???
    implicit val xil/*<-local2*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
    var yl/*<-local3*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
    // NOTE: lazy not allowed here. Only vals can be lazy
    // lazy var yzl: Int = ???
    // NOTE: local variables cannot be final
    // final var yfl: Int = ???
    implicit var yil/*<-local4*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
    yl/*->local3*/ = xl/*->local0*/ +/*->scala::Int#`+`(+4).*/ yl/*->local3*/
    println/*->scala::Predef.println(+1).*/(xzl/*->local1*/)
    yil/*->local4*/ = xil/*->local2*/ +/*->scala::Int#`+`(+4).*/ yil/*->local4*/
  }
  println/*->scala::Predef.println(+1).*/(xzlm/*->example::Vals#xzlm.*/)
  ylm/*->example::Vals#ylm().*/ = xlm/*->example::Vals#xlm.*/ +/*->scala::Int#`+`(+4).*/ ylm/*->example::Vals#ylm().*/
}

object ValUsages/*<-example::ValUsages.*/ {
  val v/*<-example::ValUsages.v.*/: Vals/*->example::Vals#*/ = ???/*->scala::Predef.`???`().*/
  v/*->example::ValUsages.v.*/.yp/*->example::Vals#`yp_=`().*/ = v/*->example::ValUsages.v.*/.xp/*->example::Vals#xp.*/ +/*->scala::Int#`+`(+4).*/ v/*->example::ValUsages.v.*/.yp/*->example::Vals#yp().*/
  v/*->example::ValUsages.v.*/.ym/*->example::Vals#`ym_=`().*/ = v/*->example::ValUsages.v.*/.xm/*->example::Vals#xm.*/ +/*->scala::Int#`+`(+4).*/ v/*->example::ValUsages.v.*/.ym/*->example::Vals#ym().*/
  v/*->example::ValUsages.v.*/.yam/*->example::Vals#`yam_=`().*/ = v/*->example::ValUsages.v.*/.xam/*->example::Vals#xam.*/ +/*->scala::Int#`+`(+4).*/ v/*->example::ValUsages.v.*/.yam/*->example::Vals#yam().*/
  println/*->scala::Predef.println(+1).*/(v/*->example::ValUsages.v.*/.xzm/*->example::Vals#xzm.*/)
  v/*->example::ValUsages.v.*/.yfm/*->example::Vals#`yfm_=`().*/ = v/*->example::ValUsages.v.*/.xfm/*->example::Vals#xfm.*/ +/*->scala::Int#`+`(+4).*/ v/*->example::ValUsages.v.*/.yfm/*->example::Vals#yfm().*/
  v/*->example::ValUsages.v.*/.yim/*->example::Vals#`yim_=`().*/ = v/*->example::ValUsages.v.*/.xim/*->example::Vals#xim.*/ +/*->scala::Int#`+`(+4).*/ v/*->example::ValUsages.v.*/.yim/*->example::Vals#yim().*/
  v/*->example::ValUsages.v.*/.explicitSetter = 25
}
