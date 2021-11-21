import scala.util.CommandLineParser.FromString
import scala.util.Try

object myProgram:

  /**
    * Help is printed normally.
    * @param a the first argument
    * @param b the second argument
    * @param c the third argument. This one is a String
    */
  @main() def doc1(a: Int, b: Int, c: String): Unit = ()

  /**
    * Help is printed in a slightly narrow column.
    * @param a the first argument
    * @param b the second argument
    * @param c the third argument. This one is a String
    */
  @main(40) def doc2(a: Int, b: Int, c: String): Unit = ()

  /**
    * Help is printed in a very narrow column!
    * @param a the first argument
    * @param b the second argument
    * @param c the third argument. This one is a String
    */
  @main(maxLineLength = 20) def doc3(a: Int, b: Int, c: String): Unit = ()


end myProgram

object Test:
  val allClazzes: Seq[Class[?]] =
    LazyList.from(1).map(i => Try(Class.forName("doc" + i.toString))).takeWhile(_.isSuccess).map(_.get)

  def main(args: Array[String]): Unit =
    for (clazz <- allClazzes) {
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, Array("--help"))
    }
end Test
