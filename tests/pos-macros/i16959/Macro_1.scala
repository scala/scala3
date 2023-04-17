import scala.quoted.*

inline def test = ${ testImpl }

def testImpl(using Quotes) =
  import quotes.reflect.*

  val int = PackedType[Int]
  val string = PackedType[String]

  assert(Type.show[(int.U, string.U, string.U)] == "scala.Tuple3[scala.Int, java.lang.String, java.lang.String]")

  '{ () }

final class PackedType[T](using t: Type[T]):
  opaque type U = T
  given tpe: Type[U] = t
