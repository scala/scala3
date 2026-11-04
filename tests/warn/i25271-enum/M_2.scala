// A Java enum whose constants have bodies is compiled by javac into a sealed
// class with anonymous permitted subclasses (E_1$1). Read from a classfile, the
// permitted subclasses must not become children of the enum, otherwise this
// match would be reported as non-exhaustive on `E_1$1`.
object M:
  def g(e: E_1): String = e match
    case E_1.A => "A"
    case E_1.B => "B"
