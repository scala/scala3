object `implicit-match-and-inline-match` {
  import scala.compiletime._

  case class Box[T](value: T)
  implicit val ibox: Box[Int] = Box(0)

  object a {
    inline def isTheBoxInScopeAnInt = implied match {
      case _: Box[t] => inline erasedValue[t] match {
        case _: Int => true
      }
    }
    val wellIsIt = isTheBoxInScopeAnInt
  }

  object b {
    inline def isTheBoxInScopeAnInt = implied match {
      case _: Box[t] => inline 0 match {
        case _: t => true
      }
    }
    val wellIsIt = isTheBoxInScopeAnInt
  }
}
