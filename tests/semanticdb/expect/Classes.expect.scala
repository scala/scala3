package classes

class C1/*<-classes::C1#*/(val x1/*<-classes::C1#x1.*/: Int/*->scala::Int#*/) extends AnyVal/*->scala::AnyVal#*//*->scala::AnyVal#`<init>`().*/

class C2/*<-classes::C2#*/(val x2/*<-classes::C2#x2.*/: Int/*->scala::Int#*/) extends AnyVal/*->scala::AnyVal#*//*->scala::AnyVal#`<init>`().*/
object C2/*<-classes::C2.*/

case class C3/*<-classes::C3#*/(x/*<-classes::C3#x.*/: Int/*->scala::Int#*/)

case class C4/*<-classes::C4#*/(x/*<-classes::C4#x.*/: Int/*->scala::Int#*/)
object C4/*<-classes::C4.*/

object M/*<-classes::M.*/ {
  implicit class C5/*<-classes::M.C5#*/(x/*<-classes::M.C5#x.*/: Int/*->scala::Int#*/)
}

case class C6/*<-classes::C6#*/(private val x/*<-classes::C6#x.*/: Int/*->scala::Int#*/)

class C7/*<-classes::C7#*/(x/*<-classes::C7#x.*/: Int/*->scala::Int#*/)

class C8/*<-classes::C8#*/(private[this] val x/*<-classes::C8#x.*/: Int/*->scala::Int#*/)

class C9/*<-classes::C9#*/(private[this] var x/*<-classes::C9#x().*/: Int/*->scala::Int#*/)

object N/*<-classes::N.*/ {
  val anonClass/*<-classes::N.anonClass.*/ = new C7/*->classes::C7#*/(42) {
    val local/*<-local1*/ = ???/*->scala::Predef.`???`().*/
  }
  val anonFun/*<-classes::N.anonFun.*/ = List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1).map/*->scala::collection::immutable::List#map().*/ { i/*<-local2*/ =>
    val local/*<-local3*/ = 2
    local/*->local3*/ +/*->scala::Int#`+`(+4).*/ 2
  }
}
