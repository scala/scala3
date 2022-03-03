package dotty.tools.dotc

import org.junit.Test
import org.junit.Assert.assertEquals

import dotty.tools.tasty.TastyHash.pjwHash64

class TastyHashTest {
  @Test def pjwHash64Tests(): Unit = {
    testHash(0L, Array.empty)
    testHash(0L, Array(0))
    testHash(1L, Array(1))
    testHash(0x7fL, Array(Byte.MaxValue))
    testHash(0x80L, Array(Byte.MinValue))
    testHash(0x101L, Array(1, 1))
    testHash(0X10101L, Array(1, 1, 1))
    testHash(0X1010101L, Array(1, 1, 1, 1))
    testHash(0X101010101L, Array(1, 1, 1, 1, 1))
    testHash(0X202020202L, Array(2, 2, 2, 2, 2))
    testHash(0X1010101L, Array.fill(1024)(1))
    testHash(0xaafefeaaab54ffL, Array.tabulate(1024)(_.toByte))
    testHash(0x34545c16020230L, "abcdefghijklmnopqrstuvwxyz1234567890".getBytes.nn)
  }

  def testHash(expected: Long, arr: Array[Byte]): Unit = {
    assertEquals(s"0x${expected.toHexString}L", s"0x${pjwHash64(arr).toHexString}L")
  }

}
