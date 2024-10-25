import scala.language.implicitConversions

type UUID = String
object MyId:
  def fromUUID[F[_]: Functor: UUIDGen]: F[String] =
    toFunctorOps(UUIDGen[F].randomUUID).map(fromUUID) // error
  private def fromUUID(id: UUID): String = ???

object UUIDGen:
  def apply[F[_]](implicit ev: UUIDGen[F]): UUIDGen[F] = ev
trait UUIDGen[F[_]]:
  def randomUUID: F[UUID]

trait Functor[F[_]]
implicit def toFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]): Ops[F, A] { type TypeClassType = Functor[F]} =
  new Ops[F, A] { type TypeClassType = Functor[F] }

trait Ops[F[_], A] {
  type TypeClassType <: Functor[F]
  def map[B](f: A => B): F[B] = ???
}
