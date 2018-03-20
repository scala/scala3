object foo {
  abstract sealed class num
  final case class One() extends num
  final case class Bit0(a: num) extends num
  final case class Bit1(a: num) extends num

  abstract sealed class abschar
  final case class zero_char() extends abschar
  final case class Char(a: num) extends abschar

  def integer_of_char(x0: abschar): BigInt = {
    def f(x: Int): Int = x
    x0 match {
    case Char(Bit0(Bit1(Bit0(Bit1(Bit0(One())))))) => BigInt(42)
    case Char(Bit1(Bit0(Bit0(Bit1(Bit0(One())))))) => BigInt(41)
    case Char(Bit0(Bit0(Bit0(Bit1(Bit0(One())))))) => BigInt(40)
    case Char(Bit1(Bit1(Bit1(Bit0(Bit0(One())))))) => BigInt(39)
    case Char(Bit0(Bit1(Bit1(Bit0(Bit0(One())))))) => BigInt(38)
    case Char(Bit1(Bit0(Bit1(Bit0(Bit0(One())))))) => BigInt(37)
    case Char(Bit0(Bit0(Bit1(Bit0(Bit0(One())))))) => BigInt(36)
    case Char(Bit1(Bit1(Bit0(Bit0(Bit0(One())))))) => BigInt(35)
    case Char(Bit0(Bit1(Bit0(Bit0(Bit0(One())))))) => BigInt(34)
    case Char(Bit1(Bit0(Bit0(Bit0(Bit0(One())))))) => BigInt(33)
    case Char(Bit0(Bit0(Bit0(Bit0(Bit0(One())))))) => BigInt(32)
    case Char(Bit1(Bit1(Bit1(Bit1(One()))))) => BigInt(31)
    case Char(Bit0(Bit1(Bit1(Bit1(One()))))) => BigInt(30)
    case Char(Bit1(Bit0(Bit1(Bit1(One()))))) => BigInt(29)
    case Char(Bit0(Bit0(Bit1(Bit1(One()))))) => BigInt(28)
    case Char(Bit1(Bit1(Bit0(Bit1(One()))))) => BigInt(27)
    case Char(Bit0(Bit1(Bit0(Bit1(One()))))) => BigInt(26)
    case Char(Bit1(Bit0(Bit0(Bit1(One()))))) => BigInt(25)
    case Char(Bit0(Bit0(Bit0(Bit1(One()))))) => BigInt(24)
    case Char(Bit1(Bit1(Bit1(Bit0(One()))))) => BigInt(23)
    case Char(Bit0(Bit1(Bit1(Bit0(One()))))) => BigInt(22)
    case Char(Bit1(Bit0(Bit1(Bit0(One()))))) => BigInt(21)
    case Char(Bit0(Bit0(Bit1(Bit0(One()))))) => BigInt(20)
    case Char(Bit1(Bit1(Bit0(Bit0(One()))))) => BigInt(19)
    case Char(Bit0(Bit1(Bit0(Bit0(One()))))) => BigInt(18)
    case Char(Bit1(Bit0(Bit0(Bit0(One()))))) => BigInt(17)
    case Char(Bit0(Bit0(Bit0(Bit0(One()))))) => BigInt(16)
    case Char(Bit1(Bit1(Bit1(One())))) => BigInt(15)
    case Char(Bit0(Bit1(Bit1(One())))) => BigInt(14)
    case Char(Bit1(Bit0(Bit1(One())))) => BigInt(13)
    case Char(Bit0(Bit0(Bit1(One())))) => BigInt(12)
    case Char(Bit1(Bit1(Bit0(One())))) => BigInt(11)
    case Char(Bit0(Bit1(Bit0(One())))) => BigInt(10)
    case Char(Bit1(Bit0(Bit0(One())))) => BigInt(9)
    case Char(Bit0(Bit0(Bit0(One())))) => BigInt(8)
    case Char(Bit1(Bit1(One()))) => BigInt(7)
    case Char(Bit0(Bit1(One()))) => BigInt(6)
    case Char(Bit1(Bit0(One()))) => BigInt(5)
    case Char(Bit0(Bit0(One()))) => BigInt(4)
    case Char(Bit1(One())) => BigInt(3)
    case Char(Bit0(One())) => BigInt(2)
    case Char(One()) => BigInt(1)
    case zero_char() => BigInt(0)
  }
  }
}
