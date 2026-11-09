package scala.runtime

import org.junit.Assert.*
import org.junit.Test

class BoxesRunTimeTest {
  @Test def `compare null Num and Char`: Unit =
    assertFalse("null", BoxesRunTime.equalsNumChar(null, 'A'))
}
