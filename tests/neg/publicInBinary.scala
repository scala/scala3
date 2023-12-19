//> using options -Werror -WunstableInlineAccessors

package foo

import scala.annotation.publicInBinary

@publicInBinary type A // error
@publicInBinary class C: // error
  def f: Unit =
    @publicInBinary def g = () // error
    ()
class D[@publicInBinary T] // error

def f(@publicInBinary x: Int) = 3 // error

@publicInBinary enum Enum1: // error
  case A

enum Enum2:
  @publicInBinary case A // error
  @publicInBinary case B(a: Int) // error


class Bar (
  @publicInBinary x: Int, // error
  @publicInBinary private[Bar] y: Int, // error
  @publicInBinary private[Bar] val z: Int,
)
