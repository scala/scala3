package scala.math

import org.junit.Assert.*
import org.junit.Test

class NumericTest {

  /* Test for scala/bug#8102 */
  @Test
  def testAbs(): Unit = {
    assertTrue(java.lang.Double.compare((-0.0).abs, 0.0) == 0)
    assertTrue(java.lang.Float.compare((-0.0f).abs, 0.0f) == 0)
  }

  /* Test for scala/bug#9348 */
  @Test
  def testBigDecimalAsIfIntegral(): Unit = {
    val num = scala.math.Numeric.BigDecimalAsIfIntegral
    assertTrue(num.quot(BigDecimal(2.5), BigDecimal(0.5)) equals BigDecimal(5.0))
    assertTrue(num.quot(BigDecimal(5.0), BigDecimal(2.0)) equals BigDecimal(2.0))
  }}
