import scala.annotation.tailrec
@tailrec
def foo(i: Int): Int = {
  if (i > 10000000) {
    i
  } else {
    val bar: String = {
      return foo(i + 1)
      "foo"
    }
    -1
  }
}
@main def Test =
  println(foo(0))