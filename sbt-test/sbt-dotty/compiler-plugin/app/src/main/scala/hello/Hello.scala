package hello

object Hello {
  def main(args: Array[String]): Unit = {
    val dotty: Int | String = "dotty"

    val y = 5 / 0 // error
    100 + 6 / 0 // error
    6L / 0L // error
    val z = 7 / 0.0 // error

    println(s"Hello $dotty!")
  }
}
