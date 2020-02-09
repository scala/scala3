class Test1 {
  def spawn(f: => Unit)(naming: Int = 4): Unit = ???
  def test(): Unit = spawn {
    val x: Int = 5
    x
  }()
}
package X {

  import scala.concurrent._
  import scala.concurrent.duration._

  import scala.concurrent.ExecutionContext.Implicits._

  class Test1 {

    def spawn[T](f: => T)(using ec:ExecutionContext,  naming: Int = 3): Future[T] = ???

    def await[T](f:Future[T], atMost: Duration = Duration.Inf)(using ec: ExecutionContext):T = ???

    def test(): Unit = {
      val promiseToWait = Promise[Int]()
      val future1 = spawn{
        val x = await(promiseToWait.future)
        x+1
      }
    }
  }
}