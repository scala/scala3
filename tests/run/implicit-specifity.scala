case class Show[T](val i: Int)
object Show {
  def apply[T](implicit st: Show[T]): Int = st.i

  given showInt as Show[Int] = new Show[Int](0)
  given fallback[T] as Show[T] = new Show[T](1)
}

class Generic
object Generic {
  given gen as Generic = new Generic
  given showGen[T] as Show[T] given Generic = new Show[T](2)
}

class Generic2
object Generic2 {
  opaque type HiPriority = AnyRef
  given showGen[T] as (Show[T] & HiPriority) = new Show[T](2).asInstanceOf
}

class SubGen extends Generic
object SubGen {
  given as SubGen
}
object Contextual {
  trait Context
  given ctx as Context
  given showGen[T] as Show[T] given Generic = new Show[T](2)
  given showGen[T] as Show[T] given Generic, Context = new Show[T](3)
  given showGen[T] as Show[T] given SubGen = new Show[T](4)
}

object Test extends App {
  assert(Show[Int] == 0)
  assert(Show[String] == 1)
  assert(Show[Generic] == 1)   // showGen loses against fallback due to longer argument list
  assert(Show[Generic2] == 2)  // ... but the opaque type intersection trick works.
}
