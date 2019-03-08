
object Test {

  def main(args: Array[String]): Unit = {
    println(sum(1, 2, 3))
  }

  inline def sum(inline i: Int, inline j: Int, inline k: Int): Int = ${ Macros.sum(i, j, k) }
}
