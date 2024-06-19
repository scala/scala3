package a
package b

object Givens/*<-a::b::Givens.*/:

  extension [A/*<-a::b::Givens.sayHello().[A]*/](any/*<-a::b::Givens.sayHello().(any)*/: A/*->a::b::Givens.sayHello().[A]*/)
    def sayHello/*<-a::b::Givens.sayHello().*/ = s/*->scala::StringContext#s().*/"Hello, I am $any/*->a::b::Givens.sayHello().(any)*/"

  extension [B/*<-a::b::Givens.sayGoodbye().[B]*//*<-a::b::Givens.saySoLong().[B]*/](any/*<-a::b::Givens.sayGoodbye().(any)*//*<-a::b::Givens.saySoLong().(any)*/: B/*->a::b::Givens.sayGoodbye().[B]*//*->a::b::Givens.saySoLong().[B]*/)
    def sayGoodbye/*<-a::b::Givens.sayGoodbye().*/ = s/*->scala::StringContext#s().*/"Goodbye, from $any/*->a::b::Givens.sayGoodbye().(any)*/"
    def saySoLong/*<-a::b::Givens.saySoLong().*/ = s/*->scala::StringContext#s().*/"So Long, from $any/*->a::b::Givens.saySoLong().(any)*/"

  val hello1/*<-a::b::Givens.hello1.*/ = 1.sayHello/*->a::b::Givens.sayHello().*/
  val goodbye1/*<-a::b::Givens.goodbye1.*/ = 1.sayGoodbye/*->a::b::Givens.sayGoodbye().*/
  val soLong1/*<-a::b::Givens.soLong1.*/ = 1.saySoLong/*->a::b::Givens.saySoLong().*/

  trait Monoid/*<-a::b::Givens.Monoid#*/[A/*<-a::b::Givens.Monoid#[A]*/]:
    def empty/*<-a::b::Givens.Monoid#empty().*/: A/*->a::b::Givens.Monoid#[A]*/
    extension (x/*<-a::b::Givens.Monoid#combine().(x)*/: A/*->a::b::Givens.Monoid#[A]*/) def combine/*<-a::b::Givens.Monoid#combine().*/(y/*<-a::b::Givens.Monoid#combine().(y)*/: A/*->a::b::Givens.Monoid#[A]*/): A/*->a::b::Givens.Monoid#[A]*/

  given Monoid/*->a::b::Givens.Monoid#*/[String/*->scala::Predef.String#*/] with
    def empty/*<-a::b::Givens.given_Monoid_String.empty().*/ = ""
    extension (x/*<-a::b::Givens.given_Monoid_String.combine().(x)*/: String/*->scala::Predef.String#*/) def combine/*<-a::b::Givens.given_Monoid_String.combine().*/(y/*<-a::b::Givens.given_Monoid_String.combine().(y)*/: String/*->scala::Predef.String#*/) = x/*->a::b::Givens.given_Monoid_String.combine().(x)*/ +/*->java::lang::String#`+`().*/ y/*->a::b::Givens.given_Monoid_String.combine().(y)*/

  inline given int2String/*<-a::b::Givens.int2String().*/: Conversion/*->scala::Conversion#*/[Int/*->scala::Int#*/, String/*->scala::Predef.String#*/] with
    def apply/*<-a::b::Givens.int2String#apply().*/(x/*<-a::b::Givens.int2String#apply().(x)*/: Int/*->scala::Int#*/): String/*->scala::Predef.String#*/ = x/*->a::b::Givens.int2String#apply().(x)*/.toString/*->scala::Any#toString().*/

  def foo/*<-a::b::Givens.foo().*/[A/*<-a::b::Givens.foo().[A]*/](using A/*<-a::b::Givens.foo().(A)*/: Monoid/*->a::b::Givens.Monoid#*/[A/*->a::b::Givens.foo().[A]*/]): A/*->a::b::Givens.foo().[A]*/ = A/*->a::b::Givens.foo().(A)*/.combine/*->a::b::Givens.Monoid#combine().*/(A/*->a::b::Givens.foo().(A)*/.empty/*->a::b::Givens.Monoid#empty().*/)(A/*->a::b::Givens.foo().(A)*/.empty/*->a::b::Givens.Monoid#empty().*/)
