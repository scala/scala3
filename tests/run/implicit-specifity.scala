case class Show[T](val i: Int)
object Show {
  def apply[T](implicit st: Show[T]): Int = st.i

  implied showInt for Show[Int] = new Show[Int](0)
  implied fallback[T] for Show[T] = new Show[T](1)
}

class Generic
object Generic {
  implied gen for Generic = new Generic
  implied showGen[T] given Generic for Show[T] = new Show[T](2)
}

object Contextual {
  trait Context
  implied ctx for Context
  implied showGen[T] given Generic for Show[T] = new Show[T](2)
  implied showGen[T] given Generic, Context for Show[T] = new Show[T](3)
}

object Test extends App {
  assert(Show[Int] == 0)
  assert(Show[String] == 1)
  assert(Show[Generic] == 2) // showGen beats fallback due to longer argument list

  { import implied Contextual._
    assert(Show[Generic] == 3)
  }
}
