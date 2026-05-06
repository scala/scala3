import scala.language.implicitConversions

// https://github.com/scala/scala-collection-contrib/blob/main/src/main/scala/scala/collection/decorators/package.scala
object decorators {
  trait IsMap[A]
  implicit def mapDecorator[C](coll: C)(implicit map: IsMap[C]): Map[C, map.type] = ???
}
import decorators.mapDecorator // unused, required to reproduce

trait Eq[T]
trait Applicative[F[_]]
given Applicative[Option] = ???

trait Traverse[F[_]]:
  // context bound required to reproduce
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = ???

object Traverse:
  def apply[F[_]]: Traverse[F] = ???

trait Segment[Element: Eq]

case class MergeResult[Element: Eq] private (segments: Seq[Segment[Element]]):
  def thisFailsToCompile(): Option[MergeResult[Element]] =
    Traverse[Seq]
      .sequence(Seq.empty[Option[Segment[Element]]])
      .map(MergeResult.apply) // no error
