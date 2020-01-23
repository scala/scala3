enum SingleCase {
  case TheCase1(u: Unit)
}

case class TheCase2(u: Unit)

case class TheCase3(s: String, u: Unit)

class TheCase4(val u: Unit)

abstract class TheCase5(val u: Unit)

@main def Test =
  SingleCase.TheCase1(())
  TheCase2(())
  TheCase3("", ())
  TheCase4(())
  new TheCase5(()) {}
