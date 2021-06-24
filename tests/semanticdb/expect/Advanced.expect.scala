package advanced

import scala.language/*->scala::language.*/.higherKinds/*->scala::language.higherKinds.*/
import scala.language/*->scala::language.*/.reflectiveCalls/*->scala::language.reflectiveCalls.*/

import scala.reflect.Selectable/*->scala::reflect::Selectable.*/.reflectiveSelectable/*->scala::reflect::Selectable.reflectiveSelectable().*/

class C/*<-advanced::C#*/[T/*<-advanced::C#[T]*/] {
  def t/*<-advanced::C#t().*/: T/*->advanced::C#[T]*/ = ???/*->scala::Predef.`???`().*/
}

class Structural/*<-advanced::Structural#*/ {
  def s1/*<-advanced::Structural#s1().*/: { val x/*<-local1*/: Int/*->scala::Int#*/ } = ???/*->scala::Predef.`???`().*/
  def s2/*<-advanced::Structural#s2().*/: { val x/*<-local3*/: Int/*->scala::Int#*/ } = new { val x/*<-local6*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/ }
  def s3/*<-advanced::Structural#s3().*/: { def m/*<-local9*/(x/*<-local10*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/ } = new { def m/*<-local13*/(x/*<-local14*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/ }
}

class Wildcards/*<-advanced::Wildcards#*/ {
  def e1/*<-advanced::Wildcards#e1().*/: List/*->scala::package.List#*/[_] = ???/*->scala::Predef.`???`().*/
}

object Test/*<-advanced::Test.*/ {
  val s/*<-advanced::Test.s.*/ = new Structural/*->advanced::Structural#*/
  val s1/*<-advanced::Test.s1.*/ = s/*->advanced::Test.s.*/.s1/*->advanced::Structural#s1().*/
  val s1x/*<-advanced::Test.s1x.*/ = /*->scala::reflect::Selectable.reflectiveSelectable().*/s/*->advanced::Test.s.*/.s1/*->advanced::Structural#s1().*//*->scala::reflect::Selectable#selectDynamic().*/.x
  val s2/*<-advanced::Test.s2.*/ = s/*->advanced::Test.s.*/.s2/*->advanced::Structural#s2().*/
  val s2x/*<-advanced::Test.s2x.*/ = /*->scala::reflect::Selectable.reflectiveSelectable().*/s/*->advanced::Test.s.*/.s2/*->advanced::Structural#s2().*//*->scala::reflect::Selectable#selectDynamic().*/.x
  val s3/*<-advanced::Test.s3.*/ = s/*->advanced::Test.s.*/.s3/*->advanced::Structural#s3().*/
  val s3x/*<-advanced::Test.s3x.*/ = /*->scala::reflect::Selectable.reflectiveSelectable().*/s/*->advanced::Test.s.*/.s3/*->advanced::Structural#s3().*//*->scala::reflect::Selectable#applyDynamic().*/.m(???/*->scala::Predef.`???`().*/)

  val e/*<-advanced::Test.e.*/ = new Wildcards/*->advanced::Wildcards#*/
  val e1/*<-advanced::Test.e1.*/ = e/*->advanced::Test.e.*/.e1/*->advanced::Wildcards#e1().*/
  val e1x/*<-advanced::Test.e1x.*/ = e/*->advanced::Test.e.*/.e1/*->advanced::Wildcards#e1().*/.head/*->scala::collection::IterableOps#head().*/

  {
    (???/*->scala::Predef.`???`().*/ : Any/*->scala::Any#*/) match {
      case e3/*<-local19*/: List/*->scala::package.List#*/[_] =>
        val e3x/*<-local21*/ = e3/*->local19*/.head/*->scala::collection::IterableOps#head().*/
        ()
    }
  }
}
