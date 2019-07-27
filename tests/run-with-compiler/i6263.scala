import quoted._
object Test {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {
    fn("foo")
    fn((1,2))
    fn(O)
    fn(1)
  }

  def fn[T : Type](v : T) = "ok"
}

object O
