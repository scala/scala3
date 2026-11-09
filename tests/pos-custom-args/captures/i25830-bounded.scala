import language.experimental.captureChecking
import caps.*

class File extends ExclusiveCapability

def testFlat() =
  val f = { [C^, D^ <: {C}] => (xs: List[File^{D}]) => xs }
  val a = File()
  val _ : List[File^{a}] = f[{a}, {a}](List[File^{a}](a))

def testLowerBound() =
  val f = { [C^, D^ >: {C}] => (xs: List[File^{D}]) => xs }
  val a = File()
  val _ : List[File^{a}] = f[{a}, {a}](List[File^{a}](a))

def testCurriedBounded() =
  val f =
    { [C^, D^ <: {C}, E^ >: {C} <: {C, D}] =>
        (xs: List[File^{D}], ys: List[File^{C}]) =>
        (zs: List[File^{E}], ws: List[File^{C, D}]) => ()
    }
  val a = File()
  val xs: List[File^{a}] = List(a)
  val ys: List[File^{a}] = List(a)
  val zs: List[File^{a}] = List(a)
  val ws: List[File^{a}] = List(a)
  val _ : Unit = f[{a}, {a}, {a}](xs, ys)(zs, ws)
