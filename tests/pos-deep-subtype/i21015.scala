
type Init[Coll[_], A, T <: Tuple] = T match
  case EmptyTuple   => A
  case head *: rest => InitCons[Coll, A, head, rest]

type InitCons[Coll[_], A, H, Rest <: Tuple] = H match
  case Int => Init[Coll, Coll[A], Rest]
  case _   => Unit

def fillVector[A, T <: Tuple](dims: T)(x: => A): Init[Vector, A, T] =
  dims match
    case _: EmptyTuple                => x
    case (p : (head *: rest)) =>
      val (head *: rest) = p
      head match
        case size: Int => fillVector(rest)(Vector.fill(size)(x))
        case _         => ()


object Minimization:

  type M1[A] = Int match
    case 1 => M2[A]

  type M2[A] = Int match
    case 2 => M1[Option[A]]

  def m1[A](x: A): M1[A] = ???

  val _: M1[Int] = m1(1) // was error
  val _: M1[Int] = m1[Int](1) // ok
  val _: M1[Int] =
    val x = m1(1)
    x // ok

end Minimization
