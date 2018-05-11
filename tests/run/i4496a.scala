import scala.reflect.Selectable.reflectiveSelectable
class Foo1 { val a: Int = 10 }
class Foo2 { def a: Int = 10 }
class Foo3 { var a: Int = 10 }
object Test {
  def main(args: Array[String]): Unit = {
    assert((new Foo1 : {val a: Int}).a == 10)
    assert((new Foo2 : {val a: Int}).a == 10)
    assert((new Foo3 : {val a: Int}).a == 10)
  }
}
