//> using options -Werror
class Foo {
  def foo[A](lss: List[List[A]]): Unit = {
    lss match {
      case xss: List[List[A]] => // no warn erasure
    }
  }
}
