
object serializer:
  trait Reader[T]
  trait Writer[T]
  // Needs to be implicit val
  implicit val UnitReader: Reader[Unit] = ???
  implicit val StringReader: Reader[String] = ???
  // A way to derive instances needs to be available
  inline given superTypeReader: [T: scala.reflect.ClassTag] => Reader[T] = ???
import serializer.Reader

trait Codec[T]
trait Channel[F[_]]:
  def notificationStub[In: Codec](): In => F[Unit]
trait Monadic[F[_]]

sealed abstract class LSPNotification():
  type In
  given inputReader: Reader[In]

class PreparedNotification[X <: LSPNotification](val x: X, val in: x.In):
  type In = x.In

trait Communicate[F[_]]:
  def notification[X <: LSPNotification](notif: X, in: notif.In): F[Unit]

object Communicate:
  given codec: [T: Reader] => Codec[T] = ???

  def channel[F[_]: Monadic](channel: Channel[F]) =
    new Communicate[F]:
      override def notification[X <: LSPNotification](notif: X, in: notif.In): F[Unit] =
        channel.notificationStub().apply(in) // was error
