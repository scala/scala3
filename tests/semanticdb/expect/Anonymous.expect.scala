package example
import scala.language/*->scala::language.*/.higherKinds/*->scala::language.higherKinds.*/

class Anonymous/*<-example::Anonymous#*/ {
  this: Anonymous/*->example::Anonymous#*/ =>

  def locally/*<-example::Anonymous#locally().*/[A/*<-example::Anonymous#locally().[A]*/](x/*<-example::Anonymous#locally().(x)*/: A/*->example::Anonymous#locally().[A]*/): A/*->example::Anonymous#locally().[A]*/ = x/*->example::Anonymous#locally().(x)*/

  def m1/*<-example::Anonymous#m1().*/[T/*<-example::Anonymous#m1().[T]*/[_]] = ???/*->scala::Predef.`???`().*/
  def m2/*<-example::Anonymous#m2().*/: Map/*->scala::Predef.Map#*/[?, List/*->scala::package.List#*/[?]] = ???/*->scala::Predef.`???`().*/
  locally/*->example::Anonymous#locally().*/ {
    ???/*->scala::Predef.`???`().*/ match { case _: List/*->scala::package.List#*/[?] => }
  }
  locally/*->example::Anonymous#locally().*/ {
    val x/*<-local0*/: Int/*->scala::Int#*/ => Int/*->scala::Int#*/ = _ => ???/*->scala::Predef.`???`().*/
  }

  trait Foo/*<-example::Anonymous#Foo#*/
  val foo/*<-example::Anonymous#foo.*/ = /*<-local1*/new Foo/*->example::Anonymous#Foo#*/ {}

  trait Bar/*<-example::Anonymous#Bar#*/:
    def bar/*<-example::Anonymous#Bar#bar().*/: String/*->scala::Predef.String#*/
  val bar1/*<-example::Anonymous#bar1.*/: Bar/*->example::Anonymous#Bar#*/ = /*<-local4*/new Bar/*->example::Anonymous#Bar#*/ { def bar/*<-local3*/: String/*->scala::Predef.String#*/ = ???/*->scala::Predef.`???`().*/ }
  val bar2/*<-example::Anonymous#bar2.*/: Bar/*->example::Anonymous#Bar#*/ = /*<-local7*/new { def bar/*<-local6*/: String/*->scala::Predef.String#*/ = ???/*->scala::Predef.`???`().*/ }
}
