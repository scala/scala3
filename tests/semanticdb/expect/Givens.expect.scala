package a
package b

object Givens/*<-a::b::Givens.*/

  given :[A](any: A)
    de/*<-a::b::Givens.given_sayHello_of_A.*//*<-a::b::Givens.given_sayHello_of_A.sayHello().[A]*//*<-a::b::Givens.given_sayHello_of_A.sayHello().(any)*//*->a::b::Givens.given_sayHello_of_A.sayHello().[A]*/f sayHello/*<-a::b::Givens.given_sayHello_of_A.sayHello().*/ = s"Hello, I am $any/*->a::b::Givens.given_sayHello_of_A.sayHello().(any)*//*->scala::StringContext#s().*/"

  val hello1/*<-a::b::Givens.hello1.*/ = /*->a::b::Givens.given_sayHello_of_A.sayHello().*/1.sayHello

  trait Monoid/*<-a::b::Givens.Monoid#*/[A/*<-a::b::Givens.Monoid#[A]*/]
    def empty/*<-a::b::Givens.Monoid#empty().*/: A/*->a::b::Givens.Monoid#[A]*/
    def (x: A) /*<-a::b::Givens.Monoid#combine().*//*<-a::b::Givens.Monoid#combine().(x)*//*->a::b::Givens.Monoid#[A]*/combine (y/*<-a::b::Givens.Monoid#combine().(y)*/: A/*->a::b::Givens.Monoid#[A]*/): A/*->a::b::Givens.Monoid#[A]*/

  given Monoid[String]
    /*<-a::b::Givens.given_Monoid_String.*//*->a::b::Givens.Monoid#*//*->scala::Predef.String#*/def empty/*<-a::b::Givens.given_Monoid_String.empty().*/ = ""
    def (x: Str/*<-a::b::Givens.given_Monoid_String.combine().*//*<-a::b::Givens.given_Monoid_String.combine().(x)*/ing/*->scala::Predef.String#*/) combine (y/*<-a::b::Givens.given_Monoid_String.combine().(y)*/: String/*->scala::Predef.String#*/) = x/*->a::b::Givens.given_Monoid_String.combine().(x)*/ +/*->java::lang::String#`+`().*/ y/*->a::b::Givens.given_Monoid_String.combine().(y)*/

  inline given int2String/*<-a::b::Givens.int2String().*/: Conversion/*->scala::Conversion#*/[Int/*->scala::Int#*/, String/*->scala::Predef.String#*/] = _.toString/*->scala::Any#toString().*/

  def foo/*<-a::b::Givens.foo().*/[A/*<-a::b::Givens.foo().[A]*/](given A/*<-a::b::Givens.foo().(A)*/: Monoid/*->a::b::Givens.Monoid#*/[A/*->a::b::Givens.foo().[A]*/]): A/*->a::b::Givens.foo().[A]*/ = A/*->a::b::Givens.foo().(A)*/.combine/*->a::b::Givens.Monoid#combine().*/(A/*->a::b::Givens.foo().(A)*/.empty/*->a::b::Givens.Monoid#empty().*/)(A/*->a::b::Givens.foo().(A)*/.empty/*->a::b::Givens.Monoid#empty().*/)
