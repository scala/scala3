// Companion to i23611: ClassTag synthesis must instantiate a TypeVar whose
// upper bound is itself another unsolved TypeVar, when the transitive bound is
// ground. Surfaced by the dobrynya/zio-jms VirtusLab community-build run.
import scala.reflect.ClassTag

sealed trait ZIO[-R, +E, +A]
extension [R, E <: Throwable, A](self: ZIO[R, E, A])
  def refineToOrDie[E1 <: E: ClassTag]: ZIO[R, E1, A] = ???

object ZIO:
  def attemptBlocking[A](code: => A): ZIO[Any, Throwable, A] = ???

class Producer { def close(): Unit = () }
class Session:
  def createProducer(dest: Object): Producer = new Producer

sealed trait Managed[-R, +E, +A]
object Managed:
  def make[R, E, A](acquire: ZIO[R, E, A])(release: A => ZIO[R, Nothing, Unit]): Managed[R, E, A] = ???

object UIO:
  def apply(code: => Unit): ZIO[Any, Nothing, Unit] = ???

object Repro:
  def producer(session: Session) =
    Managed.make(ZIO.attemptBlocking(session.createProducer(null)).refineToOrDie)(p => UIO(p.close()))
