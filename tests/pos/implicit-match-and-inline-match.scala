object `implicit-match-and-inline-match` {
  case class Box[T](value: T)
  implicit val ibox: Box[Int] = Box(0)

  // this version does not currently work:
  //
  // object a {
  //   import scala.typelevel.erasedValue
  //   inline def isTheBoxInScopeAnInt = implicit match {
  //     case _: Box[t] => inline (??? : t) match {
  //       case _: Int => true
  //       // case _ => false
  //     }
  //   }
  //   val wellIsIt = isTheBoxInScopeAnInt
  // }

  object b {
    inline def isTheBoxInScopeAnInt = implicit match {
      case _: Box[t] => inline 0 match {
        case _: t => true
        // case _ => false
      }
    }
    val wellIsIt = isTheBoxInScopeAnInt
  }
}
