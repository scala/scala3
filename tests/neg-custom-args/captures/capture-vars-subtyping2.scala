import language.experimental.captureChecking
import caps.*

trait BoundsTest:

  trait Bar { val f: () => Unit }
  def bar(x: Bar^, y: () ->{x.f} Unit): Unit = ???

  val b: Bar^ = ???

  def testTransMixed[A^,
                    B >: CapSet <: A,
                    C >: CapSet <: CapSet^{B},
                    D >: CapSet <: C,
                    E >: CapSet <: CapSet^{D},
                    F >: CapSet <: CapSet^{A,b},
                    X >: CapSet <: CapSet^{F,D},
                    Y >: CapSet^{F} <: CapSet^{F,A,b},
                    Z >: CapSet^{b} <: CapSet^{b,Y}] =
    val e: E = ???
    val e2: CapSet^{E} = e
    val ed: D = e
    val ed2: CapSet^{D} = e
    val ec: C = e
    val ec2: CapSet^{C} = e
    val eb: B = e
    val eb2: CapSet^{B} = e
    val ea: A = e
    val ea2: CapSet^{A} = e
    val ex: X = e            // error
    val ex2: CapSet^{X} = e // error
    val f: F = ???
    val f2: CapSet^{F} = f
    val y: Y = f
    val y2: CapSet^{Y} = f
    val cb: CapSet^{b} = ???
    val z: Z = cb
    val z2: CapSet^{Z} = cb

  def callTransMixed =
    val x, y, z: Bar^ = ???
    testTransMixed[CapSet^{x,y,z}, CapSet^{x,y,z}, CapSet^{x,y,z}, CapSet^{x,y,z}, CapSet^{x,y,z}, CapSet^{x,y,z}, CapSet^{x,y,z}, CapSet^{x,y,z}, CapSet^{b,x,y,z}]
    testTransMixed[CapSet^{x,y,z}, CapSet^{x,y}, CapSet^{x,y}, CapSet^{x}, CapSet^{}, CapSet^{b,x}, CapSet^{b}, CapSet^{b,x}, CapSet^{b}]
    testTransMixed[CapSet^{x,y,z}, CapSet^{x,y}, CapSet^{x,y}, CapSet^{x}, CapSet^{}, CapSet^{b,x}, CapSet^{b}, CapSet^{b,x}, CapSet^{b,x,y,z}] // error
