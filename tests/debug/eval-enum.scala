enum A(val a: Int) extends java.lang.Enum[A]:
  case A1 extends A(1)
  case A2 extends A(2)

class B(b: String):
  private enum C(c: String):
    case C1 extends C(b)
    case C2(x: String) extends C(x)

    def m: String = b + c
  end C

  def bar: String = C.C1.m

object Test:
  def main(args: Array[String]): Unit =
    val b = new B("b")
    println(b.bar)
