trait JsonRowEntry {
  def readAs[E](implicit c: Read[E]): Option[E] = ???
}
trait Read[T]
trait Codec[T] extends Read[T]
trait CodecTypeProjection[C[_]]
object JsonTransform  {
  given SetCodec: [T, C[_]: CodecTypeProjection] => scala.Conversion[C[T], C[Set[T]]] = ???
  given SetCodecExp: [T, C[_]: CodecTypeProjection] => (codec: C[T]) => C[Set[T]] = codec
  given Codec[String] = ???
  given CodecTypeProjection[Read] = ???
}

@main def Test() = {
  import JsonTransform.given
  val tree = new JsonRowEntry {}
  tree.readAs[Set[String]]
}

trait Box[E]

trait Domain

def fun[E, D[_] <: Domain](box: Box[E])(implicit domain: D[E]): Unit = {

  val newBox: Box[E] =  ???

  fun(newBox)
}
