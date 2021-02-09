package dotty.tools.scaladoc
package linking

import org.junit.Assert.assertTrue
import org.junit.Ignore

class ExtensionTest extends DriTest("extensionDRIs")

class GivenTest extends DriTest("givenDRI")

class GenericTest extends DriTest("genericDRI")

class FunctionTest extends DriTest("functionDRI")

class NestingTest extends DriTest("nestingDRI"):
  override def assertOnDRIs(dris: Seq[DRI]) =
    dris.groupBy(_.location).foreach{ case (location, dris) =>
      assertTrue(s"Location $location has multiple dris assigned: $dris", dris.size == 1)
    }

@Ignore class ShadowingTest extends DriTest("shadowingDRI"):
  override def assertOnDRIs(dris: Seq[DRI]) =
    if (!dris.exists(_.symbolUUID.contains("findThisDeclaration"))) then
      reportError("\n\nSymbol with name `findThisDeclaration` was expected but not found\n\n")