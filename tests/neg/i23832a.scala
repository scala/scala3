//> using options -explain

// follow-up to neg/i23402*.scala

trait Special[A]

object syntax:
  given Special[Option[Long]] = ???
  given Special[Option[Int]] = ??? // error
