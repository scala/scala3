import scala.util.CommandLineParser.FromString
import scala.util.Try

class MyNumber(val value: Int):
  def +(other: MyNumber): MyNumber = MyNumber(value + other.value)

class MyGeneric[T](val value: T)

given FromString[MyNumber] with
  override def fromString(s: String): MyNumber = MyNumber(summon[FromString[Int]].fromString(s))

given FromString[MyGeneric[Int]] with
  override def fromString(s: String): MyGeneric[Int] = MyGeneric(summon[FromString[Int]].fromString(s))

object myProgram:

  /**
    * Adds two numbers.
    */
  @main def doc1(num: Int, inc: Int): Unit = ()

  /** Adds two numbers. */
  @main def doc2(num: Int, inc: Int): Unit = ()

  /**
    *                       Adds two numbers.
    */
  @main def doc3(num: Int, inc: Int): Unit = ()

  /**
    * Adds two numbers.
    *
    * @param num the first number
    * @param inc the second number
    */
  @main def doc4(num: Int, inc: Int = 1): Unit = ()

  /**
    * Adds two numbers.
    *
    * @param num the first number
    */
  @main def doc5(num: Int, inc: Int): Unit = ()

  /**
    * Adds two numbers.
    *
    * @param num
    * @param inc
    */
  @main def doc6(num: Int, inc: Int): Unit = ()

  /**
    * Adds two numbers.
    *
    * @param num the first number
    * @param inc the second number
    * @return the sum of the two numbers (not really)
    */
  @main def doc7(num: Int, inc: Int): Unit = ()

  /**
    * Adds two numbers.
    *
    * @param num                     the first number
    * @param inc           the second number
    * @return                        the sum of the two numbers (not really)
    */
  @main def doc8(num: Int, inc: Int): Unit = ()

  /**
    * Adds two numbers. Same as {{doc1}}.
    *
    * @param num the first number
    * @param inc the second number
    * @return the sum of the two numbers (not really)
    * @see {{doc1}}
    */
  @main def doc9(num: Int, inc: Int): Unit = ()

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
  @main def doc10(num: Int, inc: Int): Unit = ()

  /**
    * Adds two numbers.
    *
    * @param num the first number
    *
    * Oh, a new line!
    *
    * @param inc the second number
    *
    *                               And another one!
    */
  @main def doc11(num: Int, inc: Int): Unit = ()

  /**
    * Adds two numbers. It seems that I have a very long line of documentation and therefore might need to be cut at some point to fit a small terminal screen.
    */
  @main def doc12(num: Int, inc: Int): Unit = ()

  /**
    * Addstwonumbers.ItseemsthatIhaveaverylonglineofdocumentationandthereforemightneedtobecutatsomepointtofitasmallterminalscreen.
    */
  @main def doc13(num: Int, inc: Int): Unit = ()

  /**
    * Loudly judges the number of argument you gave to this poor function.
    */
  @main def doc14(
    arg1: String, arg2: Int, arg3: String, arg4: Int,
    arg5: String, arg6: Int, arg7: String, arg8: Int,
    arg9: String = "I", arg10: Int = 42, arg11: String = "used", arg12: Int = 0,
    arg13: String = "to", arg14: Int = 34, arg15: String = "wonder", arg16: Int*
  ): Unit = ()

  /**
    * Adds two instances of {{MyNumber}}.
    * @param myNum my first number to add
    * @param myInc my second number to add
    */
  @main def doc15(myNum: MyNumber, myInc: MyNumber): Unit = ()

  /**
    * Compares two instances of {{MyGeneric}}.
    * @param first my first element
    * @param second my second element
    */
  @main def doc16(first: MyGeneric[Int], second: MyGeneric[Int]): Unit = ()

  // This should not be printed in explain!
  @main def doc17(a: Int, b: Int, c: String): Unit = ()

end myProgram

object Test:
  def callMain1(args: Array[String]): Unit =
    val clazz = Class.forName("doc1")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  val allClazzes: Seq[Class[?]] =
    LazyList.from(1).map(i => Try(Class.forName("doc" + i.toString))).takeWhile(_.isSuccess).map(_.get)

  def callAllMains(args: Array[String]): Unit =
    for (clazz <- allClazzes) {
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, args)
    }

  def main(args: Array[String]): Unit =
    callMain1(Array("--help"))
    callMain1(Array("Some", "garbage", "before", "--help"))
    callMain1(Array("--help", "and", "some", "stuff", "after"))

    callAllMains(Array("--help"))
end Test
