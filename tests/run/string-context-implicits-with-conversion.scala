
object Lib {
  def (sc: StringContext) showMe(args: Showed*): String = sc.s(args: _*)

  opaque type Showed = String

  given [T](given show: Show[T]): Conversion[T, Showed] = x => show(x)

  trait Show[T] {
    def apply(x: T): String
  }

  given Show[Int] = x => s"Int($x)"
  given Show[String] = x => s"Str($x)"
}
object Test {
  import Lib._
  def main(args: Array[String]): Unit = {
    println(showMe"${1: Int} ${"abc": String}")
    println(showMe"${1} ${"abc"}")
    println(showMe"${1} ${println("xyz"); "xyz"}")
  }

}
