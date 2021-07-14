package example
import scala.language/*->scala::language.*/.higherKinds/*->scala::language.higherKinds.*/

class Anonymous/*<-example::Anonymous#*/ {
  this: Anonymous/*->example::Anonymous#*/ =>

  def locally/*<-example::Anonymous#locally().*/[A/*<-example::Anonymous#locally().[A]*/](x/*<-example::Anonymous#locally().(x)*/: A/*->example::Anonymous#locally().[A]*/): A/*->example::Anonymous#locally().[A]*/ = x/*->example::Anonymous#locally().(x)*/

  def m1/*<-example::Anonymous#m1().*/[T/*<-example::Anonymous#m1().[T]*/[_]] = ???/*->scala::Predef.`???`().*/
  def m2/*<-example::Anonymous#m2().*/: Map/*->scala::Predef.Map#*/[_, List/*->scala::package.List#*/[_]] = ???/*->scala::Predef.`???`().*/
  locally/*->example::Anonymous#locally().*/ {
    ???/*->scala::Predef.`???`().*/ match { case _: List/*->scala::package.List#*/[_] => }
  }
  locally/*->example::Anonymous#locally().*/ {
    val x/*<-local0*/: Int/*->scala::Int#*/ => Int/*->scala::Int#*/ = _ => ???/*->scala::Predef.`???`().*/
  }

  trait Foo/*<-example::Anonymous#Foo#*/
  val foo/*<-example::Anonymous#foo.*/ = new Foo/*->example::Anonymous#Foo#*/ {}
}
