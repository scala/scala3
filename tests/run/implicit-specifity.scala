case class Show[T](val i: Int)
object Show {
  def apply[T](implicit st: Show[T]): Int = st.i

  given Show[Int] as showInt = new Show[Int](0)
  given [T] => Show[T] as fallback = new Show[T](1)
}

class Generic
object Generic {
  given Generic as gen = new Generic
  given [T] => Generic => Show[T] as showGen = new Show[T](2)
}

class Generic2
object Generic2 {
  opaque type HiPriority = AnyRef
  given [T] => (Show[T] & HiPriority) as showGen = new Show[T](2).asInstanceOf
}

class SubGen extends Generic
object SubGen {
  given SubGen
}
object Contextual {
  trait Context

  given ctx as Context

  given [T] => Generic => Show[T] as showGen = new Show[T](2)

  given [T] => (Generic, Context) => Show[T] as showGen = new Show[T](3)

  given [T] => SubGen => Show[T] as showGen = new Show[T](4)
}

object Test extends App {
  assert(Show[Int] == 0)
  assert(Show[String] == 1)
  assert(Show[Generic] == 1)   // showGen loses against fallback due to longer argument list
  assert(Show[Generic2] == 2)  // ... but the opaque type intersection trick works.
}
