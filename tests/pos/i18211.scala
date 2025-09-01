import scala.compiletime.ops.int.*

type AnyInt[A <: Int] <: Int = A match {
  case _ => A
}

type IndexOf[A, T <: Tuple] <: Int = T match {
  case EmptyTuple => -1
  case A *: t     => 0
  case _ *: t =>
    IndexOf[A, t] match {
      case -1        => -1
      case AnyInt[a] => S[a]
    }
}

type Indexes[A, T <: Tuple]
object Indexes {
  given of[A, T <: Tuple](using IndexOf[A, T] >= 0 =:= true)(using
      index: ValueOf[IndexOf[A, T]],
      next: Indexes[A, Tuple.Drop[T, S[IndexOf[A, T]]]]
  ): Indexes[A, T] = ???

  given empty: [A, T <: Tuple] => (IndexOf[A, T] =:= -1) => Indexes[A, T] = ???
}

class GetAll[A]:
  def apply[T <: Tuple](t: T)(using indexes: Indexes[A, T]): List[A] = ???

def getAll[A]: GetAll[A] = new GetAll[A]

def test =
  // the code here is trying to get all values from a tuple that has type [X] as a list

  // this works if there are only two strings in the tuple
  getAll[String](("str1", 1, "str2", false))

  //but this not compiles if there are more than two strings in the tuple
  getAll[String](("str1", 1, "str2", false, "str3"))
