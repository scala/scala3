//> using options -language:experimental.safe

@main def Test =
  val ans = scala.util.boundary { scala.util.boundary.break(6) } // ok with new implementation
  assert(ans == 6)

  val err = boundaryOld { scala.util.boundary.break(6) } // error: @unchecked is tagged as @rejectUnsafe
  assert(err == 6)
