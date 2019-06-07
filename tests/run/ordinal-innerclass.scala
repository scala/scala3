object Test extends App {

  class A {
    class B {
      object O {
        sealed trait T
        case class C(x: Int) extends T
        object T {
          case class D() extends T
        }
      }
    }
  }

  val a = new A
  val b = new a.B
  val o = b.O
  val sum: deriving.Mirror.Sum { type MirroredMonoType = o.T }= o.T.asInstanceOf
  assert(sum.ordinal(new o.C(1)) == 0)
  assert(sum.ordinal(new o.T.D()) == 1)

  object MR {
    // from betterFiles/ManagedResource: FM should not be treated as a generic sum
    // since its children are not accessible from its companion object. If it was
    // treated as a generic sum, ExplicitOuter would crash when compiling the synthesized
    // `ordinal` method.
    sealed trait FM
    object FM {
      trait I {
        object fm1 extends FM
        object fm2 extends FM
      }
    }
  }

  trait T1
  case class A1() extends T1
}