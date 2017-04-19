

abstract class A {
  def s: Boolean = { println("s"); r }
  def r: Boolean
}

object Test extends A {
  assert({ println("assert"); r2 != s }) // s not initialized yet
  def r2: true = {
    println("r2")
    true
  }
  override val r: true = {
    println("r init")
    true
  }
  def main(args: Array[String]): Unit = {}
}
