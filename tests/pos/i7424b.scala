//> using options -Werror
object GADT {
  import =:=._

  enum =:=[A, B] {
    case Refl[C]() extends (C =:= C)
  }

  def unwrap[A,B](opt: Option[A])(using ev: A =:= Option[B]): Option[B] = ev match {
    case _: Refl[?] => opt.flatMap(identity[Option[B]])
  }
}
