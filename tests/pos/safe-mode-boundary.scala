//> using options -language:experimental.safe

@main def Test =
  // see tests/warn/safe-mode-boundary.scala where we instead use `case ls: List[Int] =>` to
  // demonstrate that the @nowarn used in boundary.apply only applies to its internals,
  // and not the argument code block.
  val ls: List[Any] = List(1,2,3)
  val total = scala.util.boundary {
    ls match
      case ls: List[Any] =>
        val res = ls.flatMap({case i: Int => Some(i); case _ => None}).sum
        scala.util.boundary.break(res)
  }
  assert(total == 6)
