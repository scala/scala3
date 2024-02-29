class Scanners {
  enum IndentWidth {
    case Run(ch: Char, n: Int)
    case Conc(l: IndentWidth, r: Run)
  }

  import IndentWidth.*

  class Scanner {
    def foo() =
      Conc(Run('a', 3), Run('b', 4))
      new LookAheadScanner

    class LookAheadScanner() extends Scanner

    foo()
  }

  val m: Int = n * 2
  val n = 10   // error
}