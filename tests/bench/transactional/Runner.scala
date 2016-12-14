package transactional
import System.nanoTime

class Runner(name: String, bench: Benchmark, expected: Int) {

  val numIters = 10000000
  val numTests = 5

  def run(): Unit = {
    val start = nanoTime
    var cnt = 0
    var i = 0
    while (i < numIters) {
      cnt += bench.run()
      i += 1
    }
    assert(cnt == expected * numIters)
    val duration = nanoTime - start
    println(s"$name in ${duration / 1000000}ms")
  }

  def main(args: Array[String]) =
    for (i <- 0 until numTests)
      run()
}
