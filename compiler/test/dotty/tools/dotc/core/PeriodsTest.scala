package dotty.tools.dotc.core

import org.junit.Assert.*
import org.junit.Test

class PeriodsTest:
  import Periods._

  val p1 = Period(1, 2, 7)
  val p2 = Period(1, 3, 7)

  @Test def containsSubsetIsTrue() =
    assertTrue(p1.contains(p2))

  @Test def containsSelfIsTrue() =
    assertTrue(p1.contains(p1))

  @Test def containsSupersetIsFalse() =
    assertFalse(p2.contains(p1))

  @Test def containsInvalidIsFalse() =
    assertFalse(p1.contains(Period(0, 3, 3)))

  @Test def containsDifferentRunIsFalse() =
    assertFalse(p1.contains(Period(2, 3, 3)))

  @Test def containsNowhereIsFalse() =
    assertFalse(p1.contains(Nowhere))

  @Test def overlapsSelfIsTrue() =
    assertTrue(p1.overlaps(p1))

  @Test def overlapsOverlappingIsTrue() =
    assertTrue(Period(1, 2, 7).overlaps(Period(1, 6, 9)))

  @Test def overlapsNonOverlappingIsFalse() =
    assertFalse(Period(1, 2, 5).overlaps(Period(1, 6, 9)))

  @Test def overlapsDifferentRunIsFalse() =
    assertFalse(Period(1, 2, 7).overlaps(Period(2, 6, 9)))
