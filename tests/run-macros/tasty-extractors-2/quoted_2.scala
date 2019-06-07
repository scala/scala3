
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    printTree { var x = 1; x = 2 }
    printTree((x: Int) => x)
    printTree(???)
    printTree(1: 1)
    printTree(1: Int)
    printTree(Nil: List[Int])
    printTree(new Baz: Foo & Bar)
    printTree(1: Int | String)
    printTree { class Foo }
    printTree { object Foo }
    printTree { type Foo }
    printTree { type Foo = Int }
    printTree { type Foo >: Null <: Object }
    printTree { class Foo { @volatile var a = 0 } }
    printTree { class Foo { final def a = 0 } }
    printTree { class Foo { private[Foo] def a = 0 } }
    printTree { class Foo { protected[Foo] def a = 0 } }
    printTree { case class Foo() }
    printTree { class Foo1(a: Int) }
    printTree { class Foo2(val b: Int) }
    printTree { class Foo3(a: Int = 5) }
    printTree { class Foo4(a: Int)(b: Int) }
    printTree { class Foo5(a: Int)(b: Int = a) }
    printTree { class Foo6(a: Int)(b: a.type) }
    printTree { class Foo7(a: Int) { def this() = this(6) } }
    printTree { class Foo8 { println(0) } }
    printTree { class Foo10 { val a = 9 } }
    printTree { class Foo11 { var a = 10 } }
    printTree { class Foo12 { lazy val a = 11 } }
    printTree { class Foo; class Bar extends Foo }
    printTree { trait Foo2; class Bar extends Foo2 }
    printTree { class Foo(i: Int); class Bar extends Foo(1) }
    printTree { class Foo { type X = Int }; def f(a: Foo): a.X = ??? }
    printTree { class Foo { type X }; def f(a: Foo { type X = Int }): a.X = ??? }
    printTree { val lambda: Int => Int = x => x }
  }
}

trait Foo
trait Bar
class Baz extends Foo with Bar
