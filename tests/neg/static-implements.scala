import annotation.static

abstract class A { def x: Int }

class T
object T extends A {
  @static override val x = 10 // error: static methods cannot implement stuff
  def main(args: Array[String]): Unit = {
    println((this: A).x)
  }
}
