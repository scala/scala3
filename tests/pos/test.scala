object test {
  
  import java.util.concurrent.{ Future, Callable, ExecutorService }
  def callable[T](body: => T): Callable[T] = new Callable[T] { override def call() = body }
  
  
  def xsubmit(x: Runnable): Future[_] = ???
  //def xsubmit[T](x: Runnable, y: T): Future[T] = ???
  def xsubmit[U](x: Callable[U]): Future[U] = ???
  def spawn0[V](body: V): Future[V]         = xsubmit(callable(body))
}