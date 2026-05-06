import java.io.{File, IOException}
import java.net.URI
import java.nio.file.{Path, Paths}
import scala.reflect.ClassTag

trait FileConnectors {
  def listPath(path: => Path): ZStream[Any, IOException, Path]

  final def listFile(file: => File): ZStream[Any, IOException, File] =
    for {
      path <- null.asInstanceOf[ZStream[Any, IOException, Path]]
      r    <- listPath(path).mapZIO(a => ZIO.attempt(a.toFile).refineToOrDie)
    } yield r
}

sealed abstract class CanFail[-E]
object CanFail:
  given [E]: CanFail[E] = ???

sealed trait ZIO[-R, +E, +A]
extension [R, E <: Throwable, A](self: ZIO[R, E, A])
  def refineToOrDie[E1 <: E: ClassTag](using CanFail[E]): ZIO[R, E1, A] = ???

object ZIO:
  def attempt[A](code: => A): ZIO[Any, Throwable, A] = ???

sealed trait ZStream[-R, +E, +A]:
  def map[B](f: A => B): ZStream[R, E, B] = ???
  def flatMap[R1 <: R, E1 >: E, B](f: A => ZStream[R1, E1, B]): ZStream[R1, E1, B]
  def mapZIO[R1 <: R, E1 >: E, A1](f: A => ZIO[R1, E1, A1]): ZStream[R1, E1, A1]