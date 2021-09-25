package example

import scala.language/*->scala::language.*/.implicitConversions/*->scala::language.implicitConversions.*/

class ImplicitConversion/*<-example::ImplicitConversion#*/ {
  import ImplicitConversion/*->example::ImplicitConversion.*/.*
  implicit def string2Number/*<-example::ImplicitConversion#string2Number().*/(
      string/*<-example::ImplicitConversion#string2Number().(string)*/: String/*->scala::Predef.String#*/
  ): Int/*->scala::Int#*/ = 42
  val message/*<-example::ImplicitConversion#message.*/ = ""
  val number/*<-example::ImplicitConversion#number.*/ = 42
  val tuple/*<-example::ImplicitConversion#tuple.*/ = (1, 2)
  val char/*<-example::ImplicitConversion#char.*/: Char/*->scala::Char#*/ = 'a'

  // extension methods
  message/*->example::ImplicitConversion#message.*/
    .stripSuffix/*->scala::collection::StringOps#stripSuffix().*/("h")
  tuple/*->example::ImplicitConversion#tuple.*/ +/*->example::ImplicitConversion.newAny2stringadd#`+`().*/ "Hello"

  // implicit conversions
  val x/*<-example::ImplicitConversion#x.*/: Int/*->scala::Int#*/ = message/*->example::ImplicitConversion#message.*/

  // interpolators
  s"Hello $message/*->example::ImplicitConversion#message.*/ $number/*->example::ImplicitConversion#number.*/"/*->scala::StringContext#s().*/
  s"""Hello
     |$message/*->example::ImplicitConversion#message.*/
     |$number/*->example::ImplicitConversion#number.*/"""/*->scala::StringContext#s().*/.stripMargin/*->scala::collection::StringOps#stripMargin(+1).*/

  val a/*<-example::ImplicitConversion#a.*/: Int/*->scala::Int#*/ = char/*->example::ImplicitConversion#char.*/
  val b/*<-example::ImplicitConversion#b.*/: Long/*->scala::Long#*/ = char/*->example::ImplicitConversion#char.*/
}

object ImplicitConversion/*<-example::ImplicitConversion.*/ {
  implicit final class newAny2stringadd/*<-example::ImplicitConversion.newAny2stringadd#*/[A/*<-example::ImplicitConversion.newAny2stringadd#[A]*/](private val self/*<-example::ImplicitConversion.newAny2stringadd#self.*/: A/*->example::ImplicitConversion.newAny2stringadd#[A]*/) extends AnyVal/*->scala::AnyVal#*/ {
    def +/*<-example::ImplicitConversion.newAny2stringadd#`+`().*/(other/*<-example::ImplicitConversion.newAny2stringadd#`+`().(other)*/: String/*->scala::Predef.String#*/): String/*->scala::Predef.String#*/ = String/*->java::lang::String#*/.valueOf/*->java::lang::String#valueOf().*/(self/*->example::ImplicitConversion.newAny2stringadd#self.*/) +/*->java::lang::String#`+`().*/ other/*->example::ImplicitConversion.newAny2stringadd#`+`().(other)*/
  }
}
