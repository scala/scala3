package a
package b

object Givens/*<-a::b::Givens.*/

  given extension [A](any: /*<-a::b::Givens.given_sayHello_of_A.*//*<-a::b::Givens.given_sayHello_of_A.sayHello().[A]*//*<-a::b::Givens.given_sayHello_of_A.sayHello().(any)*/A/*->a::b::Givens.given_sayHello_of_A.sayHello().[A]*/)
    def sayHello/*<-a::b::Givens.given_sayHello_of_A.sayHello().*/ = s"/*->scala::StringContext.apply().*/Hello, I am $any/*->a::b::Givens.given_sayHello_of_A.sayHello().(any)*//*->scala::StringContext#s().*/"

  given extension [B](any: B)/*<-a::b::Givens.given_sayGoodbye_of_B.*//*<-a::b::Givens.given_sayGoodbye_of_B.sayGoodbye().[B]*//*<-a::b::Givens.given_sayGoodbye_of_B.saySoLong().[B]*//*<-a::b::Givens.given_sayGoodbye_of_B.sayGoodbye().(any)*//*<-a::b::Givens.given_sayGoodbye_of_B.saySoLong().(any)*//*->a::b::Givens.given_sayGoodbye_of_B.sayGoodbye().[B]*//*->a::b::Givens.given_sayGoodbye_of_B.saySoLong().[B]*/
    def sayGoodbye/*<-a::b::Givens.given_sayGoodbye_of_B.sayGoodbye().*/ = s"/*->scala::StringContext.apply().*/Goodbye, from $any/*->a::b::Givens.given_sayGoodbye_of_B.sayGoodbye().(any)*//*->scala::StringContext#s().*/"
    def saySoLong/*<-a::b::Givens.given_sayGoodbye_of_B.saySoLong().*/ = s"/*->scala::StringContext.apply().*/So Long, from $any/*->a::b::Givens.given_sayGoodbye_of_B.saySoLong().(any)*//*->scala::StringContext#s().*/"

  val hello1/*<-a::b::Givens.hello1.*/ = /*->a::b::Givens.given_sayHello_of_A.sayHello().*/1.sayHello
  val goodbye1/*<-a::b::Givens.goodbye1.*/ = /*->a::b::Givens.given_sayGoodbye_of_B.sayGoodbye().*/1.sayGoodbye
  val soLong1/*<-a::b::Givens.soLong1.*/ = /*->a::b::Givens.given_sayGoodbye_of_B.saySoLong().*/1.saySoLong

  trait Monoid/*<-a::b::Givens.Monoid#*/[A/*<-a::b::Givens.Monoid#[A]*/]
    def empty/*<-a::b::Givens.Monoid#empty().*/: A/*->a::b::Givens.Monoid#[A]*/
    def (x: A) /*<-a::b::Givens.Monoid#combine().*//*<-a::b::Givens.Monoid#combine().(x)*//*->a::b::Givens.Monoid#[A]*/combine (y/*<-a::b::Givens.Monoid#combine().(y)*/: A/*->a::b::Givens.Monoid#[A]*/): A/*->a::b::Givens.Monoid#[A]*/

  given Monoid[String]
    /*<-a::b::Givens.given_Monoid_String.*//*->a::b::Givens.Monoid#*//*->scala::Predef.String#*/def empty/*<-a::b::Givens.given_Monoid_String.empty().*/ = ""
    def (x: Str/*<-a::b::Givens.given_Monoid_String.combine().*//*<-a::b::Givens.given_Monoid_String.combine().(x)*/ing/*->scala::Predef.String#*/) combine (y/*<-a::b::Givens.given_Monoid_String.combine().(y)*/: String/*->scala::Predef.String#*/) = x/*->a::b::Givens.given_Monoid_String.combine().(x)*/ +/*->java::lang::String#`+`().*/ y/*->a::b::Givens.given_Monoid_String.combine().(y)*/

  inline given int2String/*<-a::b::Givens.int2String().*/: Conversion/*->scala::Conversion#*/[Int/*->scala::Int#*/, String/*->scala::Predef.String#*/] = _.toString/*->scala::Any#toString().*/

  def foo/*<-a::b::Givens.foo().*/[A/*<-a::b::Givens.foo().[A]*/](given A/*<-a::b::Givens.foo().(A)*/: Monoid/*->a::b::Givens.Monoid#*/[A/*->a::b::Givens.foo().[A]*/]): A/*->a::b::Givens.foo().[A]*/ = A/*->a::b::Givens.foo().(A)*/.combine/*->a::b::Givens.Monoid#combine().*/(A/*->a::b::Givens.foo().(A)*/.empty/*->a::b::Givens.Monoid#empty().*/)(A/*->a::b::Givens.foo().(A)*/.empty/*->a::b::Givens.Monoid#empty().*/)
