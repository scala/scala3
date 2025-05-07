import scala.deriving.Mirror

trait A:
  type B

object Test:
  case class CC(a: A, b: a.B)

  def test1(): Unit =
    val generic = summon[Mirror.Of[CC]]
    // No language syntax for type projection of a singleton type
    // summon[generic.MirroredElemTypes =:= (A, CC#a.B)]

    val aa: A { type B = Int } = new A { type B = Int }
    val x: CC { val a: aa.type } = CC(aa, 1).asInstanceOf[CC { val a: aa.type }] // manual `tracked`

    val dependent = summon[Mirror.Of[x.type]]
    summon[dependent.MirroredElemTypes =:= (A, x.a.B)]

    assert(CC(aa, 1) == generic.fromProduct((aa, 1)))
    assert(CC(aa, 1) == dependent.fromProduct((aa, 1)))

    x match
      case CC(a, b) =>
        val a1: A = a
        // Dependent pattern matching is not currently supported
        // val b1: a1.B = b
        val b1 = b // Type is CC#a.B

  end test1

  case class CCPoly[T <: A](a: T, b: a.B)

  def test2(): Unit =
    val generic = summon[Mirror.Of[CCPoly[A]]]
    // No language syntax for type projection of a singleton type
    // summon[generic.MirroredElemTypes =:= (A, CCPoly[A]#a.B)]

    val aa: A { type B = Int } = new A { type B = Int }
    val x: CCPoly[aa.type] = CCPoly(aa, 1)

    val dependent = summon[Mirror.Of[x.type]]
    summon[dependent.MirroredElemTypes =:= (aa.type, x.a.B)]

    assert(CCPoly[A](aa, 1) == generic.fromProduct((aa, 1)))
    assert(CCPoly[A](aa, 1) == dependent.fromProduct((aa, 1)))

    x match
      case CCPoly(a, b) =>
        val a1: A = a
        // Dependent pattern matching is not currently supported
        // val b1: a1.B = b
        val b1 = b // Type is CC#a.B

  end test2

  enum Enum:
    case EC(a: A, b: a.B)

  def test3(): Unit =
    val generic = summon[Mirror.Of[Enum.EC]]
    // No language syntax for type projection of a singleton type
    // summon[generic.MirroredElemTypes =:= (A, Enum.EC#a.B)]

    val aa: A { type B = Int } = new A { type B = Int }
    val x: Enum.EC { val a: aa.type } = Enum.EC(aa, 1).asInstanceOf[Enum.EC { val a: aa.type }] // manual `tracked`

    val dependent = summon[Mirror.Of[x.type]]
    summon[dependent.MirroredElemTypes =:= (A, x.a.B)]

    assert(Enum.EC(aa, 1) == generic.fromProduct((aa, 1)))
    assert(Enum.EC(aa, 1) == dependent.fromProduct((aa, 1)))

    x match
      case Enum.EC(a, b) =>
        val a1: A = a
        // Dependent pattern matching is not currently supported
        // val b1: a1.B = b
        val b1 = b // Type is Enum.EC#a.B

  end test3

  def main(args: Array[String]): Unit =
    test1()
    test2()
    test3()
