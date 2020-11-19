package dotty.dokka.linking

import org.jetbrains.dokka.links.DRI
import org.junit.Ignore

class ExtensionTest extends DriTest("extensionDRIs")

class GivenTest extends DriTest("givenDRI")

class GenericTest extends DriTest("genericDRI")

class FunctionTest extends DriTest("functionDRI")

@Ignore class ShadowingTest extends DriTest("shadowingDRI"):
  override def assertOnDRIs(dris: Seq[DRI]) =
    if (!dris.flatMap(d => Option(d.getExtra)).exists(_.contains("findThisDeclaration"))) then
      reportError("\n\nSymbol with name `findThisDeclaration` was expected but not found\n\n")