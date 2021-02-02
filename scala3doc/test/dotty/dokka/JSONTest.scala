package dotty.dokka

import org.junit.Test
import org.junit.Assert._

class JSONTest:
  @Test
  def testStrings =
    assertEquals(quoteStr("""ala"""), jsonString("""ala"""))
    assertEquals(quoteStr("""\""""), jsonString("""""""))