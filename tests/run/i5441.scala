object Test {
  def main(args: Array[String]): Unit  =
    def a(): Float = java.lang.Float.intBitsToFloat(1079290514)
    def b(): Long = 1412906027847L
    println(b() % a())
    println((b().toFloat % a().toFloat).toFloat)
    println((b().toDouble % a().toDouble).toFloat)

  def println(x: Float): Unit =
    Console.println(x.toDouble) // portable on Scala.js
}
