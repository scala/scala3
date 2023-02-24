package foo
import scala.annotation.binaryAPI
class A:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @binaryAPI private val valBinaryAPI3: Int = 1
  @binaryAPI private[foo] val valBinaryAPI4: Int = 1
    inline def inlined =
    valBinaryAPI1 + // error
    valBinaryAPI2 + // error
    valBinaryAPI3 +
    valBinaryAPI4
class B(val a: A):
  inline def inlined =
    a.valBinaryAPI2 + // error
    a.valBinaryAPI4

final class C:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @binaryAPI private val valBinaryAPI3: Int = 1
  @binaryAPI private[foo] val valBinaryAPI4: Int = 1
    inline def inlined =
    valBinaryAPI1 + // error
    valBinaryAPI2 + // error
    valBinaryAPI3 +
    valBinaryAPI4
final class D(val c: C):
  inline def inlined =
    c.valBinaryAPI2 + // error
    c.valBinaryAPI4

object E:
  private val valBinaryAPI1: Int = 1
  private[foo] val valBinaryAPI2: Int = 1
  @binaryAPI private val valBinaryAPI3: Int = 1
  @binaryAPI private[foo] val valBinaryAPI4: Int = 1
    inline def inlined =
    valBinaryAPI1 + // error
    valBinaryAPI2 + // error
    valBinaryAPI3 +
    valBinaryAPI4
object F:
  inline def inlined =
    E.valBinaryAPI2 + // error
    E.valBinaryAPI4
