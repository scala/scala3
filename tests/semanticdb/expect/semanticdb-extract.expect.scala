object AnObject/*<-_empty_::AnObject.*/ {

  def foo/*<-_empty_::AnObject.foo().*/(x/*<-_empty_::AnObject.foo().(x)*/: Int/*->scala::Int#*/) = ()
  def foo/*<-_empty_::AnObject.foo(+1).*/(): Unit/*->scala::Unit#*/ = ()

  foo/*->_empty_::AnObject.foo().*/(1)
  foo/*->_empty_::AnObject.foo(+1).*/()

  "".substring/*->java::lang::String#substring().*/(1)
  "".substring/*->java::lang::String#substring(+1).*/(1, 2)

  List/*->scala::package.List.*/(1, 2)
  List/*->scala::package.List.*/.apply/*->scala::collection::IterableFactory#apply().*/()
  List/*->scala::package.List.*/.`apply`/*->scala::collection::IterableFactory#apply().*/()
  println/*->scala::Predef.println(+1).*/(1 +/*->scala::Int#`+`(+4).*/ 2)

  case class Foo/*<-_empty_::AnObject.Foo#*/(x/*<-_empty_::AnObject.Foo#x.*/: Int/*->scala::Int#*/)
}
