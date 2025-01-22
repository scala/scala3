
import Extensions.testExt

object Extensions:
  extension (s: String) private def testExt = s.reverse
  extension (n: Int) private def xs = "x" * n

@main def test() = println:
  val a = "abc".testExt // error
  val b = 42.xs // error
  a + b
