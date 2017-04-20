
abstract class A {
  def s: Boolean = { println("s"); r }
  def r: Boolean
}

object Test extends A {
  assert({ println("assert"); r == s }) // r constant type replaced by false
  override val r: false = {
    println("r init")
    false
  }
  def main(args: Array[String]): Unit = {}
}
