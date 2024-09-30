package unroll

object TestUtils {
  def logAssertStartsWith(actual: String, expected: String): Unit = {
    assert(actual.startsWith(expected))
    val suffix = {
      val suffix0 = actual.stripPrefix(expected)
      if (suffix0.isEmpty) "" else s""" + "$suffix0""""
    }
    println(s"""Assertion passed: found "$expected"$suffix""")
  }
}
