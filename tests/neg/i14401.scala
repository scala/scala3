object Test {
  def f: Int = x;
  val x: Int = f;

  {
    def f: Int = x; // error
    val x: Int = f;
  }
  {
    def f: Int = g; // error
    val x: Int = f;
    def g: Int = x;
  }
  {
    def f: Int = g; // error
    var x: Int = f;
    def g: Int = x;
  }
  {
    def f: Int = g;
    Console.println("foo");
    def g: Int = f;
  }
  {
    import scala.concurrent.{ExecutionContext, Future}, ExecutionContext.Implicits

    def foo: Future[Int] = {
      val fInt = Future.successful(1)
      val z = for {
        a <- fInt
      } yield a // error

      implicit val ec: ExecutionContext = Implicits.global
      z
    }
    foo
  }
}
object MyApp {
  def main(args: Array[String]) = {
    class NotUsed {val xs = args} // error
    val dummy = false
    // oops, shadows the parameter
    def args = Seq("a","b","c")
  }
}
