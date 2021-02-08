package example

import scala.language/*->scala::language.*/.implicitConversions/*->scala::language.implicitConversions.*/

class ImplicitConversion/*<-example::ImplicitConversion#*/ {
  import ImplicitConversion/*->example::ImplicitConversion.*/.*
  implicit def string2Number/*<-example::ImplicitConversion#string2Number().*/(
      string/*<-example::ImplicitConversion#string2Number().(string)*/: String/*->scala::Predef.String#*/
  ): Int/*->scala::Int#*/ = 42
  val message/*<-example::ImplicitConversion#message.*/ = ""
  val number/*<-example::ImplicitConversion#number.*/ = 42
  val tuple/*<-example::ImplicitConversion#tuple.*/ = (/*->scala::Tuple2.apply().*/1, 2)
  val char/*<-example::ImplicitConversion#char.*/: Char/*->scala::Char#*/ = 'a'

  // extension methods
  /*->scala::Predef.augmentString().*/message/*->example::ImplicitConversion#message.*/
    .stripSuffix/*->scala::collection::StringOps#stripSuffix().*/("h")
  /*->example::ImplicitConversion.newAny2stringadd().*/tuple/*->example::ImplicitConversion#tuple.*/ +/*->example::ImplicitConversion.newAny2stringadd#`+`().*/ "Hello"

  // implicit conversions
  val x/*<-example::ImplicitConversion#x.*/: Int/*->scala::Int#*/ = /*->example::ImplicitConversion#string2Number().*/message/*->example::ImplicitConversion#message.*/

  // interpolators
  s"/*->scala::StringContext.apply().*/Hello $message/*->example::ImplicitConversion#message.*/ $number/*->example::ImplicitConversion#number.*/"/*->scala::StringContext#s().*/
  /*->scala::Predef.augmentString().*/s"""/*->scala::StringContext.apply().*/Hello
     |$message/*->example::ImplicitConversion#message.*/
     |$number/*->example::ImplicitConversion#number.*/"""/*->scala::StringContext#s().*/.stripMargin/*->scala::collection::StringOps#stripMargin(+1).*/

  val a/*<-example::ImplicitConversion#a.*/: Int/*->scala::Int#*/ = /*->scala::Char.char2int().*/char/*->example::ImplicitConversion#char.*/
  val b/*<-example::ImplicitConversion#b.*/: Long/*->scala::Long#*/ = /*->scala::Char.char2long().*/char/*->example::ImplicitConversion#char.*/
}

object ImplicitConversion/*<-example::ImplicitConversion.*/ {
  implicit final class newAny2stringadd/*<-example::ImplicitConversion.newAny2stringadd#*/[A/*<-example::ImplicitConversion.newAny2stringadd#[A]*/](private val self/*<-example::ImplicitConversion.newAny2stringadd#self.*/: A/*->example::ImplicitConversion.newAny2stringadd#[A]*/) extends AnyVal/*->scala::AnyVal#*//*->scala::AnyVal#`<init>`().*/ {
    def +/*<-example::ImplicitConversion.newAny2stringadd#`+`().*/(other/*<-example::ImplicitConversion.newAny2stringadd#`+`().(other)*/: String/*->scala::Predef.String#*/): String/*->scala::Predef.String#*/ = String/*->java::lang::String#*/.valueOf/*->java::lang::String#valueOf().*/(self/*->example::ImplicitConversion.newAny2stringadd#self.*/) +/*->java::lang::String#`+`().*/ other/*->example::ImplicitConversion.newAny2stringadd#`+`().(other)*/
  }
}
