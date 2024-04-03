trait Decoder
object Decoder:
  given foo: Int = ???

type DecoderToInt[Why] >: Int <: Int

def bar[T](using d: DecoderToInt[T]): Any = ???
def test: Unit = bar[Decoder]