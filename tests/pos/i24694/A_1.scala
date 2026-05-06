trait Show[T]
object Show:
  given [A: Show]: Show[List[A]] = ???

trait Eq[A] extends Any, Serializable
object Eq:
  def fromUniversalEquals[A]: Eq[A] = ???

object expect:
  def same[A](expected: A, found: A)(using
      eqA: Eq[A] = Eq.fromUniversalEquals[A],
      showA: Show[A]
  ): Unit = ???

sealed trait XmlEvent
object XmlEvent:
  given Show[XmlEvent] = ???
