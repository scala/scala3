
object Lib {
  extension (sc: StringContext) def showMe(args: Showed*): String = sc.s(args*)

  opaque type Showed = String

  given [T] => (show: Show[T]) => Conversion[T, Showed] = x => show(x)

  trait Show[T] {
    def apply(x: T): String
  }

  given Show[Int] = x => s"Int($x)"
  given Show[String] = x => s"Str($x)"
}
object Test {
  import Lib.*
  def main(args: Array[String]): Unit = {
    println(showMe"${1: Int} ${"abc": String}")
    println(showMe"${1} ${"abc"}")
    println(showMe"${1} ${println("xyz"); "xyz"}")
  }

}
