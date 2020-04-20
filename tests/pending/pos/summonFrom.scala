// summonFroms that bind parameters don't work yet.
import compiletime.summonFrom
object summonFroms {
  object invariant {
    case class Box[T](value: T)
    implicit val box: Box[Int] = Box(0)
    transparent inline def unbox: Any = summonInline[Box[t]].value
    val i: Int = unbox
    val i2 = unbox
    val i3: Int = i2
  }

  object covariant {
    case class Box[+T](value: T)
    implicit val box: Box[Int] = Box(0)
    transparent inline def unbox: Any = summonInline[Box[t]].value
    val i: Int = unbox
    val i2 = unbox
    val i3: Int = i2
  }

  object contravariant {
    case class TrashCan[-T](trash: T => Unit)
    implicit val trashCan: TrashCan[Int] = TrashCan { i => ; }
    transparent inline def trash: Nothing => Unit = summonInline[TrashCan[t]].trash
    val t1: Int => Unit = trash
    val t2 = trash
    val t3: Int => Unit = t2
  }
}
