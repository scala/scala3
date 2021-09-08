package dotty.tools
package dotc
package transform

import org.junit.*, Assert.*

import core.*, Constants.*, Contexts.*, Decorators.*, Symbols.*, Types.*

class SpaceEngineTest extends DottyTest:
  @Test def testAdaptTest(): Unit =
    given Context = ctx
    val defn = ctx.definitions
    import defn._
    val e = patmat.SpaceEngine()

    val BoxedIntType = BoxedIntClass.typeRef
    val ConstOneType = ConstantType(Constant(1))

    assertTrue(e.isPrimToBox(IntType, BoxedIntType))
    assertFalse(e.isPrimToBox(BoxedIntType, IntType))
    assertTrue(e.isPrimToBox(ConstOneType, BoxedIntType))

    assertEquals(BoxedIntType, e.adaptType(IntType, BoxedIntType).widenSingleton)
    assertEquals(IntType,      e.adaptType(BoxedIntType, IntType).widenSingleton)
    assertEquals(IntType,      e.adaptType(BoxedIntType, ConstOneType).widenSingleton)
