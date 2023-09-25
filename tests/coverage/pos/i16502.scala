import scala.concurrent.*

def asyncSum: ExecutionContext ?=> Future[Int] = Future(1)

@main
def Test(): Unit =
  import scala.concurrent.ExecutionContext.Implicits.global
  asyncSum
