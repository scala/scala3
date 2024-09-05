package dotty.tools.benchmarks.scripts

import org.junit.{Test, Assert}
import Assert.assertEquals

class UtilsTests:

  val data = Seq(1.0, 2.0, 3.0, 4.0, 5.0)

  @Test def percentile00th() = assertEquals(1.0, data.percentile(0.0), 0.0)
  @Test def percentile25th() = assertEquals(2.0, data.percentile(0.25), 0.0)
  @Test def percentile49th() = assertEquals(3.0, data.percentile(0.499999), 0.001)
  @Test def percentile50th() = assertEquals(3.0, data.percentile(0.5), 0.0)
  @Test def percentile51th() = assertEquals(3.0, data.percentile(0.500001), 0.001)
  @Test def percentile87th() = assertEquals(4.5, data.percentile(0.875), 0.0)
  @Test def percentile93th() = assertEquals(4.75, data.percentile(0.9375), 0.0)
  @Test def percentile100th() = assertEquals(5.0, data.percentile(1.0), 0.0)
