//> using options -experimental -Yno-experimental

@addInnerClass
class Foo
  //> class Show:
  //>   def showMe(x: Foo): String = "showMe: " + x.getClass
  //> def toString(): String = (new Show).showMe(this)

@addInnerClass
object Bar
  //> class Show:
  //>   def showMe(x: Foo): String = "showMe: " + x.getClass
  //> def toString(): String = (new Show).showMe(this)

@main def Test(): Unit =
  val foo = new Foo
  assert(foo.toString() == "showMe: class Foo", foo)
  assert(Bar.toString() == "showMe: class Bar$", Bar)
