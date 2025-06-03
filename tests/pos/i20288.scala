
trait Decoder[A]
object Decoder {
  given Decoder[DataRow] = ???
  extension [A <: Tuple](d: Decoder[A])
    def ~[B](fd: Field[B]): Decoder[Tuple.Concat[A, Tuple1[B]]] = ???
}

trait Field[A]
object Field:
  val int: Field[Int] = ???
  extension [A](self: Field[A])
    def ~[B](that: Field[B])(using Decoder[DataRow]): Decoder[(A, B)] = ???

trait DataRow
def simpleQuery[S, A](query: String)(using Decoder[A]): Either[Throwable, A] = ???

@main def Test = {
  import Decoder.*
  val fails = simpleQuery("")(using
    Field.int ~ Field.int ~ Field.int
  )
}
