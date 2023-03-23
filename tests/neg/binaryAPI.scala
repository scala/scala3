package foo

import scala.annotation.binaryAPI

@binaryAPI type A // error
@binaryAPI class C: // error
  def f: Unit =
    @binaryAPI def g = () // error
    ()
class D[@binaryAPI T] // error

def f(@binaryAPI x: Int) = 3 // error

@binaryAPI enum Enum1: // error
  case A

enum Enum2:
  @binaryAPI case A // error
  @binaryAPI case B(a: Int) // error
