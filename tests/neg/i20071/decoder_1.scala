//  decoder_1.scala
trait Decoder
object Decoder:
  given foo: Int = ???

def bar(using d: M[Decoder]): Any = ???

type M[Y] = Y match
  case Decoder => Int