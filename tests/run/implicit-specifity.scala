case class Show[T](val i: Int)
object Show {
  def apply[T](implicit st: Show[T]): Int = st.i

  implied showInt for Show[Int] = new Show[Int](0)
  implied fallback[T] for Show[T] = new Show[T](1)
}

class Generic
object Generic {
  implied gen for Generic = new Generic
  implied showGen[T] for Show[T] given Generic = new Show[T](2)
}

class Generic2
object Generic2 {
  opaque type HiPriority = AnyRef
  implied showGen[T] for (Show[T] & HiPriority) = new Show[T](2).asInstanceOf
}

class SubGen extends Generic
object SubGen {
  implied for SubGen
}
object Contextual {
  trait Context
  implied ctx for Context
  implied showGen[T] for Show[T] given Generic = new Show[T](2)
  implied showGen[T] for Show[T] given Generic, Context = new Show[T](3)
  implied showGen[T] for Show[T] given SubGen = new Show[T](4)
}

object Test extends App {
  assert(Show[Int] == 0)
  assert(Show[String] == 1)
  assert(Show[Generic] == 1)   // showGen loses against fallback due to longer argument list
  assert(Show[Generic2] == 2)  // ... but the opaque type intersection trick works.
}
