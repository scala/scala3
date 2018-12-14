object `implicit-match-trivial` {
  object invariant {
    case class Box[T](value: T)
    implicit val box: Box[Int] = Box(0)
    inline def unbox <: Any = implicit match {
      case b: Box[t] => b.value : t
    }
    //val i: Int = unbox
    val i = unbox
    val i2: Int = i
  }

  object covariant {
    case class Box[+T](value: T)
    implicit val box: Box[Int] = Box(0)
    inline def unbox <: Any = implicit match {
      case b: Box[t] => b.value
    }
    //val i: Int = unbox
    val i = unbox
    val i2: Int = i
  }

  object contravariant {
    case class Eater[-T](eat: T => Unit)
    implicit val eater: Eater[Int] = Eater { i => ; }
    inline def uneater <: Nothing => Unit = implicit match {
      case b: Eater[t] => b.eat : (t => Unit)
    }
    // val eat: Int => Unit = uneater
    val eat = uneater
    val eat2: Int => Unit = eat
  }
}
