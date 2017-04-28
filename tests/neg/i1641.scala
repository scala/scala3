package bar { object bippy extends (Double => String) { def apply(x: Double): String = "Double" } }
package object println { def bippy(x: Int, y: Int, z: Int) = "(Int, Int, Int)" }
object Test {
  def main(args: Array[String]): Unit = {
    println(bar.bippy(5.5)) // error
    println(bar.bippy(1, 2, 3)) // error
  }
}
