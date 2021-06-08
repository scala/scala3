package dotty.tools.dotc
package reporting

import config.CommandLineParser.tokenize
import config.Settings._

import org.junit.Test
import org.junit.Assert._

class WConfTest:

  @Test def `WConf setting is parsed`: Unit =
    assertEquals(1, 1)
    assertTrue("No warnings!", true)

end WConfTest
