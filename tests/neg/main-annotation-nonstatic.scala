import scala.annotation.newMain

class A:
  @newMain def foo(bar: Int) = () // error
