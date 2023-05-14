
trait T
trait U

class Test(x: Any)(using val t: T = ???)(using val u: U = ???, disregard: Any = ???)

object Test {
  def apply() = new Test(???)
}
