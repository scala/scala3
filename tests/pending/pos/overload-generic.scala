class Test {
  def rawr(string: String): String = string
}

implicit final class RawrExt(val t: Test) {
  def rawr(int: Int): Int = int
}

val t = new Test

val r0 = t.rawr(5)


class TestP {
  def rawr[A](list: List[A]): List[A] = list
}

implicit final class RawrExtP(val t: TestP) {
  def rawr(int: Int): Int = int
}

val tt = new TestP

val r1 = tt.rawr(5) // This doesn't compile