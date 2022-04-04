import scala.annotation.newMain

/** Adds two numbers */
@newMain def add(num: Int, inc: Int): Unit =
  println(s"$num + $inc = ${num + inc}")

/** Adds any amount of numbers */
@newMain def addAll(num: Int = 0, incs: Int*): Unit =
  print(num)
  if (incs.length > 0) {
    print(" + ")
    print(incs.mkString(" + "))
  }
  println(s" = ${num + incs.sum}")

object Test:
  def callMainAdd(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def callMainAddAll(args: Array[String]): Unit =
    val clazz = Class.forName("addAll")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMainAdd(Array("2", "3"))

    callMainAddAll(Array("2", "3"))
    callMainAddAll(Array("2"))
    callMainAddAll(Array())
    callMainAddAll(Array("1", "2", "3", "4"))
end Test
