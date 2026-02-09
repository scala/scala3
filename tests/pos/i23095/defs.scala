import scala.compiletime.*
import scala.deriving.*

trait SeqStringCodec[A]:
  def decode(value: Seq[String]): Either[Any, A]

trait UrlForm
trait FormCodec[A]

object FormCodec {
  inline given derived[A](using p: Mirror.ProductOf[A]): FormCodec[A] =
    val codecs = summonAll[Tuple.Map[p.MirroredElemTypes, SeqStringCodec]].toArray
    val values = codecs.map {
      _.asInstanceOf[SeqStringCodec[?]]
        .decode(List.empty[String])
    }
    ???
}
