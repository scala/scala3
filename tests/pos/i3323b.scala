//> using options -Werror -deprecation -feature

class Foo {
  def foo(lss: List[Int]): Unit = {
    lss match {
      case xss: List[Int] =>
    }
  }
}
