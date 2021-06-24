package classes
import scala.language/*->scala::language.*/.experimental/*->scala::language.experimental.*/.macros/*->scala::language.experimental.macros.*/
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

class C10/*<-classes::C10#*/(s/*<-classes::C10#s.*/: => String/*->scala::Predef.String#*/)

class C11/*<-classes::C11#*/ {
  def foo/*<-classes::C11#foo().*/: Int/*->scala::Int#*/ = macro ???/*->scala::Predef.`???`().*/
  inline def foo/*<-classes::C11#foo().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
}

class C12/*<-classes::C12#*/ {

  class Context/*<-classes::C12#Context#*/: // Dummy scala.reflect.macros.Context
    type Expr/*<-classes::C12#Context#Expr#*/[T/*<-classes::C12#Context#Expr#[T]*/]

  def foo1/*<-classes::C12#foo1().*/(x/*<-classes::C12#foo1().(x)*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/ = macro foo1Impl/*->classes::C12#foo1Impl().*/
  def foo1/*<-classes::C12#foo1().*/(x/*<-classes::C12#foo1().(x)*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/

  def foo2/*<-classes::C12#foo2().*/(x/*<-classes::C12#foo2().(x)*/: Int/*->scala::Int#*/, y/*<-classes::C12#foo2().(y)*/: String/*->scala::Predef.String#*/): Int/*->scala::Int#*/ = macro foo2Impl/*->classes::C12#foo2Impl().*/
  def foo2/*<-classes::C12#foo2().*/(x/*<-classes::C12#foo2().(x)*/: Int/*->scala::Int#*/, y/*<-classes::C12#foo2().(y)*/: String/*->scala::Predef.String#*/): Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/

  def foo1Impl/*<-classes::C12#foo1Impl().*/(context/*<-classes::C12#foo1Impl().(context)*/: Context/*->classes::C12#Context#*/)(x/*<-classes::C12#foo1Impl().(x)*/: context/*->classes::C12#foo1Impl().(context)*/.Expr/*->classes::C12#Context#Expr#*/[Int/*->scala::Int#*/]): context/*->classes::C12#foo1Impl().(context)*/.Expr/*->classes::C12#Context#Expr#*/[Int/*->scala::Int#*/] = ???/*->scala::Predef.`???`().*/
  def foo2Impl/*<-classes::C12#foo2Impl().*/(context/*<-classes::C12#foo2Impl().(context)*/: Context/*->classes::C12#Context#*/)(x/*<-classes::C12#foo2Impl().(x)*/: context/*->classes::C12#foo2Impl().(context)*/.Expr/*->classes::C12#Context#Expr#*/[Int/*->scala::Int#*/], y/*<-classes::C12#foo2Impl().(y)*/: context/*->classes::C12#foo2Impl().(context)*/.Expr/*->classes::C12#Context#Expr#*/[String/*->scala::Predef.String#*/]): context/*->classes::C12#foo2Impl().(context)*/.Expr/*->classes::C12#Context#Expr#*/[Int/*->scala::Int#*/] = ???/*->scala::Predef.`???`().*/

}

object N/*<-classes::N.*/ {
  val anonClass/*<-classes::N.anonClass.*/ = new C7/*->classes::C7#*/(42) {
    val local/*<-local2*/ = ???/*->scala::Predef.`???`().*/
  }
  val anonFun/*<-classes::N.anonFun.*/ = List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1).map/*->scala::collection::immutable::List#map().*/ { i/*<-local3*/ =>
    val local/*<-local4*/ = 2
    local/*->local4*/ +/*->scala::Int#`+`(+4).*/ 2
  }
}
