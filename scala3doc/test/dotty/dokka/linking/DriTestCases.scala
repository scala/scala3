package dotty.dokka
package linking
import org.junit.Assert.assertTrue

import org.junit.Ignore

class ExtensionTest extends DriTest("extensionDRIs")

class GivenTest extends DriTest("givenDRI")

class GenericTest extends DriTest("genericDRI")

class FunctionTest extends DriTest("functionDRI")

class NestingTest extends DriTest("nestingDRI"):
  override def assertOnDRIs(dris: Seq[DRI]) =
    println(dris.groupBy(_.location).map(_._1))
    dris.groupBy(_.location).foreach{ case (location, dris) =>
      assertTrue(s"Location $location has multiple dris assigned: $dris", dris.size == 1)
    }

@Ignore class ShadowingTest extends DriTest("shadowingDRI"):
  override def assertOnDRIs(dris: Seq[DRI]) =
    if (!dris.flatMap(d => Option(d.getExtra)).exists(_.contains("findThisDeclaration"))) then
      reportError("\n\nSymbol with name `findThisDeclaration` was expected but not found\n\n")