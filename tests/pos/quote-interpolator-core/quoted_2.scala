
import FInterpolation._

object Test {
  println(ff"integer: ${5}%d")
  println(ff"string: ${"l"}%s")
  println(ff"${5}%s, ${6}%d, ${"hello"}%s")
  val hello = "hello"
  println(ff"${5}%s, ${6}%d, ${hello}%s")
  println(ff"${5}%s, ${6}%d, ${FInterpolation.hello}%s")
}