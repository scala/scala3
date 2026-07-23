import language.experimental.captureChecking

def double(x: Int): Int = x * 2

case class Box(value: Int)

def test(s: String): Unit =
  val a: Int with a == double(2) = double(2)
  val n: Int with n == s.length = s.length
  val b: Box with b.value == 1 = Box(1)
  val c: Int with c > 0 = 1
