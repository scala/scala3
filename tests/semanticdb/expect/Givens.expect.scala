package a
package b

object Givens/*<-a::b::Givens.*/:

  extension [A/*<-a::b::Givens.extension_sayHello().[A]*/](any/*<-a::b::Givens.extension_sayHello().(any)*/: A/*->a::b::Givens.extension_sayHello().[A]*/)
    def sayHello/*<-a::b::Givens.extension_sayHello().*/ = s"/*->scala::StringContext.apply().*/Hello, I am $any/*->a::b::Givens.extension_sayHello().(any)*/"/*->scala::StringContext#s().*/

  extension [B/*<-a::b::Givens.extension_sayGoodbye().[B]*//*<-a::b::Givens.extension_saySoLong().[B]*/](any/*<-a::b::Givens.extension_sayGoodbye().(any)*//*<-a::b::Givens.extension_saySoLong().(any)*/: B/*->a::b::Givens.extension_sayGoodbye().[B]*//*->a::b::Givens.extension_saySoLong().[B]*/)
    def sayGoodbye/*<-a::b::Givens.extension_sayGoodbye().*/ = s"/*->scala::StringContext.apply().*/Goodbye, from $any/*->a::b::Givens.extension_sayGoodbye().(any)*/"/*->scala::StringContext#s().*/
    def saySoLong/*<-a::b::Givens.extension_saySoLong().*/ = s"/*->scala::StringContext.apply().*/So Long, from $any/*->a::b::Givens.extension_saySoLong().(any)*/"/*->scala::StringContext#s().*/

  val hello1/*<-a::b::Givens.hello1.*/ = /*->a::b::Givens.extension_sayHello().*/1.sayHello
  val goodbye1/*<-a::b::Givens.goodbye1.*/ = /*->a::b::Givens.extension_sayGoodbye().*/1.sayGoodbye
  val soLong1/*<-a::b::Givens.soLong1.*/ = /*->a::b::Givens.extension_saySoLong().*/1.saySoLong

  trait Monoid/*<-a::b::Givens.Monoid#*/[A/*<-a::b::Givens.Monoid#[A]*/]:
    def empty/*<-a::b::Givens.Monoid#empty().*/: A/*->a::b::Givens.Monoid#[A]*/
    extension (x/*<-a::b::Givens.Monoid#extension_combine().(x)*/: A/*->a::b::Givens.Monoid#[A]*/) def combine/*<-a::b::Givens.Monoid#extension_combine().*/(y/*<-a::b::Givens.Monoid#extension_combine().(y)*/: A/*->a::b::Givens.Monoid#[A]*/): A/*->a::b::Givens.Monoid#[A]*/

  given Monoid[String]:
   /*<-a::b::Givens.given_Monoid_String.*//*->a::b::Givens.Monoid#*//*->scala::Predef.String#*/ def empty/*<-a::b::Givens.given_Monoid_String.empty().*/ = ""
    extension (x/*<-a::b::Givens.given_Monoid_String.extension_combine().(x)*/: String/*->scala::Predef.String#*/) def combine/*<-a::b::Givens.given_Monoid_String.extension_combine().*/(y/*<-a::b::Givens.given_Monoid_String.extension_combine().(y)*/: String/*->scala::Predef.String#*/) = x/*->a::b::Givens.given_Monoid_String.extension_combine().(x)*/ +/*->java::lang::String#`+`().*/ y/*->a::b::Givens.given_Monoid_String.extension_combine().(y)*/

  inline given int2String/*<-a::b::Givens.int2String().*/ as Conversion/*->scala::Conversion#*/[Int/*->scala::Int#*/, String/*->scala::Predef.String#*/] = _.toString/*->scala::Any#toString().*/

  def foo/*<-a::b::Givens.foo().*/[A/*<-a::b::Givens.foo().[A]*/](using A/*<-a::b::Givens.foo().(A)*/: Monoid/*->a::b::Givens.Monoid#*/[A/*->a::b::Givens.foo().[A]*/]): A/*->a::b::Givens.foo().[A]*/ = A/*->a::b::Givens.foo().(A)*/.extension_combine/*->a::b::Givens.Monoid#extension_combine().*/(A/*->a::b::Givens.foo().(A)*/.empty/*->a::b::Givens.Monoid#empty().*/)(A/*->a::b::Givens.foo().(A)*/.empty/*->a::b::Givens.Monoid#empty().*/)
