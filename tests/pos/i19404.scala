given ipEncoder[IP <: IpAddress]: Encoder[IP] = Encoder[String].contramap(_.toString)

class Encoder[A] {
  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B]
}

object Encoder {
  final def apply[A](implicit instance: Encoder[A]): Encoder[A] = instance
  implicit final val encodeString: Encoder[String] = new Encoder[String]
}

trait Json
trait IpAddress