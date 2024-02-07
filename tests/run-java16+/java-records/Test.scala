// scalajs: --skip

object Test:
  def main(args: Array[String]): Unit =
    val r0 = R0()
    r0 match
      case R0() =>

    val r1 = R1(42)
    r1 match
      case R1(i) => assert(i == 42)

    val R1(i) = r1
    assert(i == 42)
