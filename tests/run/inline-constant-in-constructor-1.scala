

abstract class A {
  def s: Boolean = { println("s"); r }
  def r: Boolean
}

object Test extends A {
  assert({ println("assert"); r == s }) // r constant type not replaced by true, r not initialized yet
  override val r: true = {
    println("r init")
    true
  }
  def main(args: Array[String]): Unit = {}
}
