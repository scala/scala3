trait Sam1:
  type T
  def apply(x: T): T

trait Sam2:
  var x: Int = 1 // To force anonymous class generation
  type T
  def apply(x: T): T

object Test:
  def main(args: Array[String]): Unit =
    val s1: Sam1 { type T = String } = x => x.trim
    s1.apply("foo")
    val s2: Sam2 { type T = Int } = x => x + 1
    s2.apply(1)
