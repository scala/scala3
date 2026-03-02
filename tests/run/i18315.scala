trait Sam1:
  type T
  def apply(x: T): T

trait Sam2:
  var x: Int = 1 // To force anonymous class generation
  type T
  def apply(x: T): T

trait Sam3:
  type T
  type U
  def apply(x: T): U

object Test:
  def main(args: Array[String]): Unit =
    val s1: Sam1 { type T = String } = x => x.trim
    s1.apply("foo")
    val s2: Sam2 { type T = Int } = x => x + 1
    s2.apply(1)
    val s3: Sam3 { type T = Int; type U = String } = x => x.toString
    s3.apply(2)
