import dotty.tools.dotc.core.tasty.TastyHash.pjwHash64

object Test {
  def main(args: Array[String]): Unit = {
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
    testHash(0X55aa01fe55ab54ffL, Array.tabulate(1024)(_.toByte))
    testHash(0x34545c16020230L, "abcdefghijklmnopqrstuvwxyz1234567890".getBytes)
  }

  def testHash(expected: Long, arr: Array[Byte]): Unit = {
    val res = pjwHash64(arr)
    assert(res == expected, s"Exprected 0x${expected.toHexString}L but got 0X${res.toHexString}L")
  }

}
