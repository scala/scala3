@main def Test: Unit = {

  val tup0: Tuple = Tuple()
  val tup1: Tuple = 1 *: Tuple()
  val tup2: Tuple = 1 *: 2 *: Tuple()

  tup0 match
    case head *: tail => assert(false)
    case Tuple() => // ok

  tup1 match
    case Tuple() => assert(false)
    case (x) => // ok

  tup2 match
    case Tuple() => assert(false)
    case (1, 2) => // ok

}
