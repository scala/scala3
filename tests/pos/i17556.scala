sealed trait A {
  // must be `object` or `case class`
  object X extends A
  case class Y() extends A
}

// companion object must exist
object A