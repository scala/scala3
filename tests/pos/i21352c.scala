
trait Text[T]
trait Read[A]
object Read extends ReadImplicits:
  implicit val unit: Read[Unit] = ???
trait ReadImplicits:
  import scala.deriving.*
  given roe: Read[Option[EmptyTuple]] = ???
  given rou: Read[Option[Unit]] = ???
  given cons1[H, T <: Tuple](using Read[Option[H]], Read[Option[T]]): Read[Option[H *: T]] = ???

trait Fragment:
  def query[B: Read]: String = ???

@main def Test =
  val f: Fragment = ???
  f.query // was error
