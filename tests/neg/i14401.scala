object Test:
  def f: Int = x
  val x: Int = f

  locally:
    def f: Int = x // error
    val x: Int = f

  locally:
    def f: Int = g // error
    val x: Int = f
    def g: Int = x

  locally:
    def f: Int = g // error
    var x: Int = f
    def g: Int = x

  locally:
    def f: Int = g
    Console.println("foo")
    def g: Int = f

  locally:
    import scala.concurrent.{ExecutionContext, Future}, ExecutionContext.Implicits

    def foo: Future[Int] =
      val fInt = Future.successful(42)
      val z =
        for a <- fInt
        yield a + 27 // error
      implicit val ec: ExecutionContext = Implicits.global
      z
    foo

object MyApp:
  def main(args: Array[String]) =
    class NotUsed {val xs = args} // error
    val dummy = false
    // oops, shadows the parameter
    def args = Seq("a","b","c")
