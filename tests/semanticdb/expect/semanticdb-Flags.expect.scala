package flags

import scala.language/*->scala::language.*/.experimental/*->scala::language.experimental.*/.macros/*->scala::language.experimental.macros.*/

package object p/*<-flags::p::package.*/ {
  private lazy val x/*<-flags::p::package.x.*/ = 1
  protected implicit var y/*<-flags::p::package.y().*/: Int/*->scala::Int#*/ = 2
  def z/*<-flags::p::package.z().*/(pp/*<-flags::p::package.z().(pp)*/: Int/*->scala::Int#*/) = 3
  def m/*<-flags::p::package.m().*/[TT/*<-flags::p::package.m().[TT]*/]: Int/*->scala::Int#*/ = macro ???/*->scala::Predef.`???`().*/
  abstract class C/*<-flags::p::package.C#*/[+T/*<-flags::p::package.C#[T]*/, -U/*<-flags::p::package.C#[U]*/, V/*<-flags::p::package.C#[V]*/](x/*<-flags::p::package.C#x.*/: T/*->flags::p::package.C#[T]*/, y/*<-flags::p::package.C#y.*/: U/*->flags::p::package.C#[U]*/, z/*<-flags::p::package.C#z.*/: V/*->flags::p::package.C#[V]*/) {
    def this() = this(???/*->scala::Predef.`???`().*/, ???/*->scala::Predef.`???`().*/, ???/*->scala::Predef.`???`().*/)
    def this(t/*<-flags::p::package.C#`<init>`(+2).(t)*/: T/*->flags::p::package.C#[T]*/) = this(t/*->flags::p::package.C#`<init>`(+2).(t)*/, ???/*->scala::Predef.`???`().*/, ???/*->scala::Predef.`???`().*/)
    def w/*<-flags::p::package.C#w().*/: Int/*->scala::Int#*/
  }
  type T1/*<-flags::p::package.T1#*/ = Int/*->scala::Int#*/
  type T2/*<-flags::p::package.T2#*/[T/*<-flags::p::package.T2#[T]*/] = S/*->flags::p::package.S#*/[T/*->flags::p::package.T2#[T]*/]
  type U/*<-flags::p::package.U#*/ <: Int/*->scala::Int#*/
  type V/*<-flags::p::package.V#*/ >: Int/*->scala::Int#*/
  case object X/*<-flags::p::package.X.*/
  final class Y/*<-flags::p::package.Y#*/
  sealed trait Z/*<-flags::p::package.Z#*/
  class AA/*<-flags::p::package.AA#*/(x/*<-flags::p::package.AA#x.*/: Int/*->scala::Int#*/, val y/*<-flags::p::package.AA#y.*/: Int/*->scala::Int#*/, var z/*<-flags::p::package.AA#z().*/: Int/*->scala::Int#*/)
  class S/*<-flags::p::package.S#*/[@specialized/*->scala::specialized#*/ T/*<-flags::p::package.S#[T]*/]
  val List/*->scala::package.List.*/(xs1/*<-flags::p::package.xs1.*/) = ???/*->scala::Predef.`???`().*/
  ???/*->scala::Predef.`???`().*/ match { case List/*->scala::package.List.*/(xs2/*<-local0*/) => ???/*->scala::Predef.`???`().*/ }
  ???/*->scala::Predef.`???`().*/ match { case _: List/*->scala::package.List#*/[t/*<-local1*/] => ???/*->scala::Predef.`???`().*/ }
}
