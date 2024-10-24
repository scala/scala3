import java.util.ArrayList
def f() =
  val j = new J()
  val s2 = j.foo(null)
  s2 match
    case s3: ArrayList[ArrayList[String]] => s3.get(0) match
      case _: ArrayList[_] =>
      case _ => // warn
    case _ => println(2) // warn

