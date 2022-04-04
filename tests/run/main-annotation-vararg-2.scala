// Sample main method
object myProgram:

  /** Checks that the correct amount of parameters were passed */
  @main def count(count: Int, elems: String*): Unit =
    if (elems.length == count)
      println("Correct")
    else
      println(s"Expected $count argument${if (count != 1) "s" else ""}, but got ${elems.length}")
      println(s"  ${elems.mkString(", ")}")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("count")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("1", "Hello"))
    callMain(Array("2", "Hello", "world!"))
    callMain(Array("3", "No 3 elements"))
    callMain(Array("0"))
    callMain(Array("0", "I", "shouldn't", "be", "here"))
    callMain(Array("-2", "How does that make sense?"))
    callMain(Array("26") ++ ('a' to 'z').toArray.map(_.toString))
end Test
