package dotty.tools.dotc.core

import Names.*
import Decorators.*

import org.junit.Test
import org.junit.Assert.*

class NameTest:
  @Test def test(): Unit = {
    val n = termName("hello")
    val tn = n.toTypeName
    assert(tn.toTermName eq n)
    assertFalse(n == tn)
    assertFalse(n eq tn)
    assertFalse(n == EmptyTermName)
  }