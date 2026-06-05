package advanced

import scala.language/*->scala::language.*/.higherKinds/*->scala::language.higherKinds.*/
import scala.language/*->scala::language.*/.reflectiveCalls/*->scala::language.reflectiveCalls.*/

import scala.reflect.Selectable/*->scala::reflect::Selectable.*/.reflectiveSelectable/*->scala::reflect::Selectable.reflectiveSelectable().*/

class C/*<-advanced::C#*/[T/*<-advanced::C#[T]*/] {
  def t/*<-advanced::C#t().*/: T/*->advanced::C#[T]*/ = ???/*->scala::Predef.`???`().*/
}

class Structural/*<-advanced::Structural#*/ {
  def s1/*<-advanced::Structural#s1().*/: { val x/*<-local0*/: Int/*->scala::Int#*/ } = ???/*->scala::Predef.`???`().*/
  def s2/*<-advanced::Structural#s2().*/: { val x/*<-local1*/: Int/*->scala::Int#*/ } = /*<-local3*/new { val x/*<-local2*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/ }
  def s3/*<-advanced::Structural#s3().*/: { def m/*<-local6*/(x/*<-local5*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/ } = /*<-local9*/new { def m/*<-local8*/(x/*<-local7*/: Int/*->scala::Int#*/): Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/ }
  def s4/*<-advanced::Structural#s4().*/(a/*<-advanced::Structural#s4().(a)*/: Int/*->scala::Int#*/): { val x/*<-local11*/: Int/*->scala::Int#*/ } = ???/*->scala::Predef.`???`().*/
  trait T/*<-advanced::Structural#T#*/[A/*<-advanced::Structural#T#[A]*/] { val foo/*<-advanced::Structural#T#foo.*/: { type B/*<-local12*/ = A/*->advanced::Structural#T#[A]*/ } = ???/*->scala::Predef.`???`().*/; def bar/*<-advanced::Structural#T#bar().*/(b/*<-advanced::Structural#T#bar().(b)*/: foo/*->advanced::Structural#T#foo.*/.B/*->local12*/) = () } // from tests/pos/t8177e.scala
}

class Wildcards/*<-advanced::Wildcards#*/ {
  def e1/*<-advanced::Wildcards#e1().*/: List/*->scala::package.List#*/[_] = ???/*->scala::Predef.`???`().*/
  def e2/*<-advanced::Wildcards#e2().*/: List/*->scala::package.List#*/[_ <: Int/*->scala::Int#*/] = ???/*->scala::Predef.`???`().*/
}

object Test/*<-advanced::Test.*/ {
  val s/*<-advanced::Test.s.*/ = new Structural/*->advanced::Structural#*/
  val s1/*<-advanced::Test.s1.*/ = s/*->advanced::Test.s.*/.s1/*->advanced::Structural#s1().*/
  val s1x/*<-advanced::Test.s1x.*/ = s/*->advanced::Test.s.*/.s1/*->advanced::Structural#s1().*/.x/*->local13*/
  val s2/*<-advanced::Test.s2.*/ = s/*->advanced::Test.s.*/.s2/*->advanced::Structural#s2().*/
  val s2x/*<-advanced::Test.s2x.*/ = s/*->advanced::Test.s.*/.s2/*->advanced::Structural#s2().*/.x/*->local14*/
  val s3/*<-advanced::Test.s3.*/ = s/*->advanced::Test.s.*/.s3/*->advanced::Structural#s3().*/
  val s3x/*<-advanced::Test.s3x.*/ = s/*->advanced::Test.s.*/.s3/*->advanced::Structural#s3().*/.m/*->local15*/(???/*->scala::Predef.`???`().*/)

  val e/*<-advanced::Test.e.*/ = new Wildcards/*->advanced::Wildcards#*/
  val e1/*<-advanced::Test.e1.*/ = e/*->advanced::Test.e.*/.e1/*->advanced::Wildcards#e1().*/
  val e1x/*<-advanced::Test.e1x.*/ = e/*->advanced::Test.e.*/.e1/*->advanced::Wildcards#e1().*/.head/*->scala::collection::IterableOps#head().*/

  {
    (???/*->scala::Predef.`???`().*/ : Any/*->scala::Any#*/) match {
      case e3/*<-local16*/: List/*->scala::package.List#*/[_] =>
        val e3x/*<-local18*/ = e3/*->local16*/.head/*->scala::collection::IterableOps#head().*/
        ()
    }
  }

  // see: https://github.com/scala/scala3/pull/14608#discussion_r835642563
  lazy val foo/*<-advanced::Test.foo.*/: (reflect.Selectable/*->scala::reflect::Selectable#*/ { type A/*<-local19*/ = Int/*->scala::Int#*/ }) &/*->scala::`&`#*/ (reflect.Selectable/*->scala::reflect::Selectable#*/ { type A/*<-local20*/ = Int/*->scala::Int#*/; val a/*<-local21*/: A/*->local20*/ }) = ???/*->scala::Predef.`???`().*/
  def bar/*<-advanced::Test.bar().*/: foo/*->advanced::Test.foo.*/.A/*->local20*/ = foo/*->advanced::Test.foo.*/.a/*->scala::reflect::Selectable#selectDynamic().*/
}


// Curried Type Application
class HKClass/*<-advanced::HKClass#*/[F/*<-advanced::HKClass#[F]*/ <: [T/*<-advanced::HKClass#`<init>`().[F][T]*/] =>> [U/*<-advanced::HKClass#`<init>`().[F][U]*/] =>> (U/*->advanced::HKClass#`<init>`().[F][U]*/, T/*->advanced::HKClass#`<init>`().[F][T]*/)] {
  def foo/*<-advanced::HKClass#foo().*/[T/*<-advanced::HKClass#foo().[T]*/,U/*<-advanced::HKClass#foo().[U]*/](x/*<-advanced::HKClass#foo().(x)*/: F/*->advanced::HKClass#[F]*/[T/*->advanced::HKClass#foo().[T]*/][U/*->advanced::HKClass#foo().[U]*/]): String/*->scala::Predef.String#*/ = x/*->advanced::HKClass#foo().(x)*/.toString/*->scala::Tuple2#toString().*/()
}
