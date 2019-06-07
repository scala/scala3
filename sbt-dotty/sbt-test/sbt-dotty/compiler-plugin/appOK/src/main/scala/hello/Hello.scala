package hello

object Hello {
  def main(args: Array[String]): Unit = {
    val dotty: Int | String = "dotty"
    println(s"Hello $dotty!")
  }
}
