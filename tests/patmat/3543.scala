class Test {
  class Foo {
    def unapply(x: String): Option[String] = ???
  }

  def test(xs: List[String]): Unit = {
    val Yes = new Foo
    val No = new Foo

    xs match {
      case Yes(x) :: ls => println("Yes")
      case No(y) :: ls => println("No")
      case _ =>
    }
  }
}

class Test2 {
  class Foo(x: Boolean) {
    def unapply(y: String): Boolean = x
  }

  def test(xs: List[String]): Unit = {
    val Yes = new Foo(true)
    val No = new Foo(false)

    xs match {
      case No() :: ls => println("No")
      case Yes() :: ls => println("Yes")
      case _ =>
    }
  }
}

class Test3 {
  import scala.util.matching.Regex

  def main(args: Array[String]): Unit = {
    foo("c" :: Nil, false)
  }

  def foo(remaining: List[String], inCodeBlock: Boolean): Unit = {
    remaining match {
      case CodeBlockEndRegex(before) :: ls =>
      case SymbolTagRegex(name) :: ls if !inCodeBlock => println("OK")
      case _ =>
    }
  }

  val CodeBlockEndRegex = new Regex("(b)")
  val SymbolTagRegex = new Regex("(c)")
}
