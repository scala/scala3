//> using options -language:experimental.safe

@main def Test =
  // boundary.apply uses @nowarn to avoid the "cannot be checked at runtime" warning, so
  // check here that the argument to boundary.apply can still emit the warning.
  val ls: List[Any] = List(1,2,3)
  val total = scala.util.boundary {
    ls match
      case ls: List[Int] => scala.util.boundary.break(ls.sum) // warn
  }
  assert(total == 6)
