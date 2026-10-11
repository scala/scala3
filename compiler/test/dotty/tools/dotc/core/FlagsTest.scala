package dotty.tools.dotc.core

import Flags.*

import org.junit.Test
import org.junit.Assert.*

class FlagsTest:
  private val pri = Private
  private val pro = Protected
  private val pripro = pri | pro

  @Test def test(): Unit =
    assertTrue(pripro.is(pri))
    assertTrue(pripro.is(pro))
    assertFalse(pripro.is(Local))

    val pp = pri | pro
    assertTrue(pripro.isAllOf(pp))
    assertFalse(pri.isAllOf(pp))
    assertFalse(pri.isAllOf(pripro))

    assertFalse(Method == Abstract)
