// scalac: -Werror -deprecation -feature
class Foo {
  def foo[A](lss: List[List[A]]): Unit = {
    lss match {
      case xss: List[List[A]] =>
    }
  }
}
