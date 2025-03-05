trait Invariant[T]

trait Foo
trait Bar[T]

trait Worker:
  def schedule(command: Foo): Invariant[?]
  def schedule[V](callable: Bar[V]): Invariant[V]

object Test:
  def test(worker: Worker, foo: Foo): Option[Unit] =
    Some(worker.schedule(foo))
    // error: Found: Foo
    //        Required: Bar[V]
