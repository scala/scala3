import language.experimental.captureChecking
import caps.*

trait BoundsTest:

  trait Bar { val f: () => Unit }
  def bar(x: Bar^, y: () ->{x.f} Unit): Unit = ???

  val b: Bar^ = ???

  def testTransMixed[cap A,
                     cap B <: {A},
                     cap C <: {B},
                     cap D <: {C},
                     cap E <: {D},
                     cap F <: {A,b},
                     cap X <: {F,D},
                     cap Y >: {F} <: {F,A,b},
                     cap Z >: {b} <: {b,Y}, T <: List[Bar^{Z}]] =
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
    testTransMixed[{x,y,z}, {x,y,z}, {x,y,z}, {x,y,z}, {x,y,z}, {x,y,z}, {x,y,z}, {x,y,z}, {b,x,y,z}, List[Bar^{b}]]
    testTransMixed[{x,y,z}, {x,y}, {x,y}, {x}, {}, {b,x}, {b}, {b,x}, {b}, List[Bar^{b}]]
    testTransMixed[{x,y,z}, {x,y}, {x,y}, {x}, {}, {b,x}, {b}, {b,x}, {b,x,y,z}, List[Bar^{}]] // error
