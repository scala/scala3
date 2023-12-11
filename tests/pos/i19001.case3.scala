trait IO[A]:
  def map[B](f: A => B): IO[B] = ???

trait RenderResult[T]:
  def value: T

def IOasync[T](f: (Either[Throwable, T] => Unit) => Unit): IO[T] = ???

def render[T]: IO[T] = {
  def register(cb: Either[Throwable, RenderResult[T]] => Unit): Unit = ???
  IOasync(register).map(_.value) // map should take RenderResult[T], but uses Any
}
