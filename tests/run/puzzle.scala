// no longer a puzzler, now prints 5, 5
object Test {

  def main(args: Array[String]): Unit = {
    println(if (false) 5.0 else '5')
    val x = if (false) 5.0 else '5'
    println(x)
    val z = 1L
    val y: Float = z
  }

}
