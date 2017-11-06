package dotty.tools.dotc.util

class PriorityGraph[T <: AnyRef](elems: Array[T], cmp: (T, T) => Int) {
  private val N = elems.length

  private val edge = Array.ofDim[Boolean](N, N)

  private def compare(i: Int, j: Int): Int = {
    var k = 0
    while (k < N) {
      if (edge(i)(k) && edge(k)(j)) return 1
      k += 1
    }
    k = 0
    while (k < N) {
      if (edge(j)(k) && edge(k)(i)) return -1
      k += 1
    }
    cmp(elems(i), elems(j))
  }

  /** Construct `edge` array such that
   *
   *      edge(i)(j)  ==  i != j && cmp(elems(i), elems(j))
   *
   *  Make use of the fact that `cmp` is transitive to avoid evaluations
   *  of `cmp` (which is supposed to be expensive).
   */
  private def build() =
    for (i <- 0 until N)
      for (j <- 0 until N)
        if (i != j)
          compare(i, j) match {
            case  1 => edge(i)(j) = true
            case -1 => edge(j)(i) = true
            case  0 =>
          }

  /** Reduce `edge` array to sparsest array that has
   *  the same transitive closure as the original `edge` array.
   */
  def reduce() =
    for (i <- 0 until N)
      for (j <- 0 until N) {
        var k = 0
        while (k < N && !(edge(i)(k) && edge(k)(j)))
          k += 1
        if (k < N) edge(i)(j) = false
    }

  println(s"building ${elems.length} ${elems.toList}")
  build()
  println(s"reducing ${elems.toList}")
  reduce()
  println(s"starting ${elems.toList}")

  private val indegree = {
    val deg = new Array[Int](N)
    for (i <- 0 until N) {
      var d = 0
      for (j <- 0 until N)
        if (edge(j)(i)) d += 1
      deg(i) = d
    }
    deg
  }

  private val curSources = new Array[Int](N)
  private var firstSource = 0
  private var lastSource = 0

  private def considerAsSource(n: Int) =
    if (indegree(n) == 0) {
      curSources(lastSource) = n
      lastSource += 1
    }

  for (i <- 0 until N) considerAsSource(i)

  def hasNextSource() = firstSource < lastSource

  def nextSource(): Int = {
    val n = curSources(firstSource)
    firstSource += 1
    n
  }

  def dropLastSource() = {
    val i = firstSource - 1
    for (j <- 0 until N)
      if (edge(i)(j)) {
        indegree(j) -= 1
        considerAsSource(j)
      }
  }
}