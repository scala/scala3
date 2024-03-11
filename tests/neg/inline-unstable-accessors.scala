//> using options -experimental -Yno-experimental -Werror -WunstableInlineAccessors -explain

package foo
import scala.annotation.publicInBinary
class A:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // warn
      valBinaryAPI2 + // warn
      valBinaryAPI3
class B(val a: A):
  inline def inlined =
    a.valBinaryAPI2 + // warn
    a.valBinaryAPI3

final class C:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // warn
      valBinaryAPI2 + // warn
      valBinaryAPI3
final class D(val c: C):
  inline def inlined =
    c.valBinaryAPI2 + // warn
    c.valBinaryAPI3

object E:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // warn
      valBinaryAPI2 + // warn
      valBinaryAPI3
object F:
  inline def inlined =
    E.valBinaryAPI2 + // warn
    E.valBinaryAPI3

package object G:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // warn
      valBinaryAPI2 + // warn
      valBinaryAPI3
package object H:
  inline def inlined =
    // G.valBinaryAPI2 + // FIXME should error (now fails -Ycheck)
    G.valBinaryAPI3

package I:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @publicInBinary private[foo] val valBinaryAPI3: Int = 1
    inline def inlined =
      valBinaryAPI1 + // warn
      valBinaryAPI2 + // warn
      valBinaryAPI3
package J:
  inline def inlined =
    //  I.valBinaryAPI2 + // FIXME should error (now fails -Ycheck)
    I.valBinaryAPI3

// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
