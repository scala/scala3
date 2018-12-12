
import Asserts._

object Test {
  def main(args: Array[String]): Unit = {
    assert(zeroLastArgs(-1) == -1)
    assert(zeroLastArgs(f) == 41)
    assert(zeroLastArgs(f0()) == 42)
    assert(zeroLastArgs(f1(2)) == 1)
    assert(zeroLastArgs(f2(2, 3)) == 2)
    assert(zeroLastArgs(f3(2)(4, 5)) == 5)

    assert(zeroAllArgs(-1) == -1)
    assert(zeroAllArgs(f) == 41)
    assert(zeroAllArgs(f0()) == 42)
    assert(zeroAllArgs(f1(2)) == 1)
    assert(zeroAllArgs(f2(2, 3)) == 2)
    assert(zeroAllArgs(f3(2)(4, 5)) == 3)
  }

  def f: Int = 41
  def f0(): Int = 42
  def f1(i: Int): Int = 1 + i
  def f2(i: Int, j: Int): Int = 2 + i + j
  def f3(i: Int)(j: Int, k: Int): Int = 3 + i + j
  def f4(i: Int, j: Int)(k: Int, l: Int): Int = 4 + i + j + k + l

}
