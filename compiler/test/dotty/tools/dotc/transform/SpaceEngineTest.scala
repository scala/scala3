package dotty.tools
package dotc
package transform

import org.junit.*, Assert.*

import core.*, Contexts.*, Decorators.*, Symbols.*, Types.*

class SpaceEngineTest extends DottyTest:
  @Test def testAdaptTest(): Unit =
    given Context = ctx
    val defn = ctx.definitions
    import defn._
    val e = patmat.SpaceEngine()

    val BoxedIntType = BoxedIntClass.typeRef

    assertTrue(e.isPrimToBox(IntType, BoxedIntType))
    assertFalse(e.isPrimToBox(BoxedIntType, IntType))
