object A/*<<=A.*/ {

  def foo/*<<=A.foo().*/(x/*<<=A.foo().(x)*/: Int/*=>>scala.Int#*/) = ()
  def foo/*<<=A.foo(+1).*/(): Unit/*=>>scala.Unit#*/ = ()

  foo/*=>>A.foo().*/(1)
  foo/*=>>A.foo(+1).*/()

  "".substring/*=>>java.lang.String#substring().*/(1)
  "".substring/*=>>java.lang.String#substring(+1).*/(1, 2)

  List/*=>>scala.package.List().*//*=>>scala.collection.IterableFactory#apply().*/(1, 2)
  List/*=>>scala.package.List().*/.apply/*=>>scala.collection.IterableFactory#apply().*/()
  List/*=>>scala.package.List().*/.`apply`/*=>>scala.collection.IterableFactory#apply().*/()
  println/*=>>scala.Predef.println(+1).*/(1 +/*=>>scala.Int#`+`(+4).*/ 2)

  case class Foo/*<<=A.Foo#*/(x/*<<=A.Foo#x.*/: Int/*=>>scala.Int#*/)
}