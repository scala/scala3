object Foo {
  type NonNeg = {v: Int with v >= 0}
  class IntArray(val length: NonNeg, init: Int) {
    private val data = Array.fill(length)(init)

    def access(i: {v: NonNeg with v < this.length}): Int =
      data(i)
  }

  def arrFold[A](f: (A, Int) => A, arr: IntArray, z: A): A = {
    def rec(i: NonNeg, acc: A): A =
      if (i + 1 >= 0 && i < arr.length) rec(i + 1, f(acc, arr.access(i)))
      else acc
    rec(0, z)
  }

  def arrSum(arr: IntArray): Int =
    arrFold[Int](_ + _, arr, 0)

  def max(x: Int, y: Int): {v: Int with v >= x && v >= y} =
    if (x > y) x else y

  def arrMax(arr: IntArray): NonNeg =
    arrFold[NonNeg](max, arr, 0)
}
