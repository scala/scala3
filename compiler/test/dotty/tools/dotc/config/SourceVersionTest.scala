package dotty.tools.dotc.config

import dotty.tools.dotc.core.Decorators.*

import org.junit.Test
import org.junit.Assert.*

import SourceVersionTest.*

class SourceVersionTest:

  @Test def `importedVersion`: Unit =
    assertEquals(SourceVersion.`3.0-migration`, importLanguageDot("3.0-migration"))
    assertEquals(SourceVersion.`3.0`, importLanguageDot("3.0"))

    // crucial that here `import scala.language.3.1-migration` sets the source version to `3.1`
    assertEquals(SourceVersion.`3.1`, importLanguageDot("3.1-migration"))

    assertEquals(SourceVersion.`3.1`, importLanguageDot("3.1"))
    // assertEquals(SourceVersion.`3.2-migration`, importLanguageDot("3.2-migration")) // uncomment when we introduce `3.2-migration`
    assertEquals(SourceVersion.`3.2`, importLanguageDot("3.2-migration")) // delete when we introduce `3.2-migration`
    assertEquals(SourceVersion.`3.2`, importLanguageDot("3.2"))
    assertEquals(SourceVersion.`future-migration`, importLanguageDot("future-migration"))
    assertEquals(SourceVersion.`future`, importLanguageDot("future"))

  @Test def `series`: Unit =
    assertEquals(SourceVersion.`3.0`, SourceVersion.`3.0-migration`.series)
    assertEquals(SourceVersion.`3.0`, SourceVersion.`3.0`.series)
    assertEquals(SourceVersion.`3.0`, SourceVersion.`3.1`.series)
    // assertEquals(SourceVersion.`3.2`, SourceVersion.`3.2-migration`.series) // uncomment when we introduce `3.2-migration`
    // assertEquals(SourceVersion.`3.2`, SourceVersion.`3.2`.series) // uncomment when we introduce `3.2-migration`
    assertEquals(SourceVersion.`3.0`, SourceVersion.`3.2`.series) // delete when we introduce `3.2-migration`
    assertEquals(SourceVersion.`future`, SourceVersion.`future-migration`.series)
    assertEquals(SourceVersion.`future`, SourceVersion.`future`.series)

  @Test def `isAtLeast 3.0`: Unit =
    // trues
    assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.2`)) // delete when we introduce `3.2-migration`
    assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.1`)) // delete when we introduce `3.2-migration`
    assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.0`)) // delete when we introduce `3.2-migration`
    assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.0-migration`)) // delete when we introduce `3.2-migration`
    assertTrue(SourceVersion.`3.1`.isAtLeast(SourceVersion.`3.2`)) // delete when we introduce `3.2-migration`
    assertTrue(SourceVersion.`3.1`.isAtLeast(SourceVersion.`3.1`))
    assertTrue(SourceVersion.`3.1`.isAtLeast(SourceVersion.`3.0`))
    assertTrue(SourceVersion.`3.1`.isAtLeast(SourceVersion.`3.0-migration`))
    assertTrue(SourceVersion.`3.0`.isAtLeast(SourceVersion.`3.2`)) // delete when we introduce `3.2-migration`
    assertTrue(SourceVersion.`3.0`.isAtLeast(SourceVersion.`3.1`))
    assertTrue(SourceVersion.`3.0`.isAtLeast(SourceVersion.`3.0`))
    assertTrue(SourceVersion.`3.0`.isAtLeast(SourceVersion.`3.0-migration`))
    assertTrue(SourceVersion.`3.0-migration`.isAtLeast(SourceVersion.`3.2`)) // delete when we introduce `3.2-migration`
    assertTrue(SourceVersion.`3.0-migration`.isAtLeast(SourceVersion.`3.1`))
    assertTrue(SourceVersion.`3.0-migration`.isAtLeast(SourceVersion.`3.0`))
    assertTrue(SourceVersion.`3.0-migration`.isAtLeast(SourceVersion.`3.0-migration`))


    // falses
    assertFalse(SourceVersion.`3.0-migration`.isAtLeast(SourceVersion.`future`))
    assertFalse(SourceVersion.`3.0-migration`.isAtLeast(SourceVersion.`future-migration`))
    // assertFalse(SourceVersion.`3.0-migration`.isAtLeast(SourceVersion.`3.2`)) // uncomment when we introduce `3.2-migration`
    // assertFalse(SourceVersion.`3.0-migration`.isAtLeast(SourceVersion.`3.2-migration`)) // uncomment when we introduce `3.2-migration`
    assertFalse(SourceVersion.`3.0`.isAtLeast(SourceVersion.`future`))
    assertFalse(SourceVersion.`3.0`.isAtLeast(SourceVersion.`future-migration`))
    // assertFalse(SourceVersion.`3.0`.isAtLeast(SourceVersion.`3.2`)) // uncomment when we introduce `3.2-migration`
    // assertFalse(SourceVersion.`3.0`.isAtLeast(SourceVersion.`3.2-migration`)) // uncomment when we introduce `3.2-migration`
    assertFalse(SourceVersion.`3.1`.isAtLeast(SourceVersion.`future`))
    assertFalse(SourceVersion.`3.1`.isAtLeast(SourceVersion.`future-migration`))
    // assertFalse(SourceVersion.`3.1`.isAtLeast(SourceVersion.`3.2`)) // uncomment when we introduce `3.2-migration`
    // assertFalse(SourceVersion.`3.1`.isAtLeast(SourceVersion.`3.2-migration`)) // uncomment when we introduce `3.2-migration
    assertFalse(SourceVersion.`3.2`.isAtLeast(SourceVersion.`future`)) // delete when we introduce `3.2-migration`
    assertFalse(SourceVersion.`3.2`.isAtLeast(SourceVersion.`future-migration`)) // delete when we introduce `3.2-migration`

  // @Test def `isAtLeast 3.2`: Unit = // uncomment when we introduce `3.2-migration`
    // trues
    // assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.2`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.2-migration`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.1`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.0`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2`.isAtLeast(SourceVersion.`3.0-migration`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2-migration`.isAtLeast(SourceVersion.`3.2`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2-migration`.isAtLeast(SourceVersion.`3.2-migration`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2-migration`.isAtLeast(SourceVersion.`3.1`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2-migration`.isAtLeast(SourceVersion.`3.0`)) // uncomment when we introduce `3.2-migration`
    // assertTrue(SourceVersion.`3.2-migration`.isAtLeast(SourceVersion.`3.0-migration`)) // uncomment when we introduce `3.2-migration`
    // falses
    // assertFalse(SourceVersion.`3.2-migration`.isAtLeast(SourceVersion.`future`)) // uncomment when we introduce `3.2-migration`
    // assertFalse(SourceVersion.`3.2-migration`.isAtLeast(SourceVersion.`future-migration`)) // uncomment when we introduce `3.2-migration`
    // assertFalse(SourceVersion.`3.2`.isAtLeast(SourceVersion.`future`)) // uncomment when we introduce `3.2-migration`
    // assertFalse(SourceVersion.`3.2`.isAtLeast(SourceVersion.`future-migration`)) // uncomment when we introduce `3.2-migration`

  @Test def `isAtLeast future`: Unit =
    // trues
    assertTrue(SourceVersion.`future`.isAtLeast(SourceVersion.`future`))
    assertTrue(SourceVersion.`future`.isAtLeast(SourceVersion.`future-migration`))
    assertTrue(SourceVersion.`future`.isAtLeast(SourceVersion.`3.2`))
    // assertTrue(SourceVersion.`future`.isAtLeast(SourceVersion.`3.2-migration`))
    assertTrue(SourceVersion.`future`.isAtLeast(SourceVersion.`3.1`))
    assertTrue(SourceVersion.`future`.isAtLeast(SourceVersion.`3.0`))
    assertTrue(SourceVersion.`future`.isAtLeast(SourceVersion.`3.0-migration`))
    assertTrue(SourceVersion.`future-migration`.isAtLeast(SourceVersion.`future`))
    assertTrue(SourceVersion.`future-migration`.isAtLeast(SourceVersion.`future-migration`))
    assertTrue(SourceVersion.`future-migration`.isAtLeast(SourceVersion.`3.2`))
    // assertTrue(SourceVersion.`future-migration`.isAtLeast(SourceVersion.`3.2-migration`))
    assertTrue(SourceVersion.`future-migration`.isAtLeast(SourceVersion.`3.1`))
    assertTrue(SourceVersion.`future-migration`.isAtLeast(SourceVersion.`3.0`))
    assertTrue(SourceVersion.`future-migration`.isAtLeast(SourceVersion.`3.0-migration`))
    // no falses

object SourceVersionTest:

  def importLanguageDot(feature: String): SourceVersion =
    SourceVersion.lookupSourceVersion.fromSetting(feature)
