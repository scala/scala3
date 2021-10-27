// Sample main method
object myProgram:

  /**
    * Adds two numbers.
    */
  @main def doc1(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /** Adds two numbers. */
  @main def doc2(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    *                       Adds two numbers.
    */
  @main def doc3(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Adds two numbers.
    *
    * @param num the first number
    * @param inc the second number
    */
  @main def doc4(num: Int, inc: Int = 1): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Adds two numbers.
    *
    * @param num the first number
    */
  @main def doc5(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Adds two numbers.
    *
    * @param num
    * @param inc
    */
  @main def doc6(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Adds two numbers.
    *
    * @param num the first number
    * @param inc the second number
    * @return the sum of the two numbers (not really)
    */
  @main def doc7(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Adds two numbers.
    *
    * @param num                     the first number
    * @param inc           the second number
    * @return                        the sum of the two numbers (not really)
    */
  @main def doc8(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Adds two numbers. Same as {{doc1}}.
    *
    * @param num the first number
    * @param inc the second number
    * @return the sum of the two numbers (not really)
    * @see {{doc1}}
    */
  @main def doc9(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Adds two numbers.
    *
    * This should be on another line.
    *
    *
    *
    *
    * And this also.
    *
    *
    * @param num I might have to write this
    * on two lines
    * @param inc I might even
    * have to write this one
    * on three lines
    */
  @main def doc10(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Adds two numbers. It seems that I have a very long line of documentation and therefore might need to be cut at some point to fit a small terminal screen.
    */
  @main def doc11(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /**
    * Addstwonumbers.ItseemsthatIhaveaverylonglineofdocumentationandthereforemightneedtobecutatsomepointtofitasmallterminalscreen.
    */
  @main def doc12(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

object Test:
  def callMain1(args: Array[String]): Unit =
    val clazz = Class.forName("doc1")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def callAllMains(args: Array[String]): Unit =
    val numberOfMains = 12
    for (i <- 1 to numberOfMains) {
      val clazz = Class.forName("doc" + i.toString)
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, args)
    }

  def main(args: Array[String]): Unit =
    callMain1(Array("--help"))
    callMain1(Array("Some", "garbage", "before", "--help"))
    callMain1(Array("--help", "and", "some", "stuff", "after"))

    callAllMains(Array("--help"))
end Test
