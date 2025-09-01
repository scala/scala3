trait Type[T]:
  type Out

type varchar

given Type[varchar]:
  type Out = String

class Placeholder[T, U]

object Placeholder:
  def apply[T](using t: Type[T]): Placeholder[T, t.Out] = new Placeholder

trait Encoder[P, X]:
  def encode(x: X): String

object Encoder:
  def apply[P, X](placeholder: P)(using e: Encoder[P, X]): X => String = e.encode

  given [T, X] => Encoder[Placeholder[T, X], X]:
    def encode(x: X): String = ???

def Test =
  // the following compiles just fine
  Encoder(new Placeholder[varchar, String])("hello")
  // the following fails
  Encoder(Placeholder[varchar])("hello")
