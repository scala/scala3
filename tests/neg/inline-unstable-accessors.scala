//> using options -Werror -WunstableInlineAccessors -explain

package foo
import scala.annotation.publicInBinary
class A:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // error
      valBinaryAPI2 + // error
      valBinaryAPI3
class B(val a: A):
  inline def inlined =
    a.valBinaryAPI2 + // error
    a.valBinaryAPI3

final class C:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // error
      valBinaryAPI2 + // error
      valBinaryAPI3
final class D(val c: C):
  inline def inlined =
    c.valBinaryAPI2 + // error
    c.valBinaryAPI3

object E:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // error
      valBinaryAPI2 + // error
      valBinaryAPI3
object F:
  inline def inlined =
    E.valBinaryAPI2 + // error
    E.valBinaryAPI3

package object G:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // error
      valBinaryAPI2 + // error
      valBinaryAPI3
package object H:
  inline def inlined =
    G.valBinaryAPI2 + // error
    G.valBinaryAPI3

package I:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // error
      valBinaryAPI2 + // error
      valBinaryAPI3
package J:
  inline def inlined =
    I.valBinaryAPI2 + // error
    I.valBinaryAPI3
