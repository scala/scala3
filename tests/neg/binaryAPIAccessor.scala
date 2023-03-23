package foo

import scala.annotation.binaryAPIAccessor

@binaryAPIAccessor type A // error
@binaryAPIAccessor class C: // error
  def f: Unit =
    @binaryAPIAccessor def g = () // error
    ()
class D[@binaryAPIAccessor T] // error

def f(@binaryAPIAccessor x: Int) = 3 // error

@binaryAPIAccessor enum Enum1: // error
  case A

enum Enum2:
  @binaryAPIAccessor case A // error
  @binaryAPIAccessor case B(a: Int) // error

class Foo @binaryAPIAccessor private (x: Int): // error
  @binaryAPIAccessor private def this(x: Int, y: Int) = this(x + y) // error

class Bar @binaryAPIAccessor private[this] (x: Int): // error
  @binaryAPIAccessor private[this] def this(x: Int, y: Int) = this(x + y) // error
