import language.experimental.captureChecking
import caps.*

def test[cap C] =
  val a: C = ???
  val b: CapSet^{C} = a
  val c: C = b
  val d: CapSet^{C, c} = a

def testTrans[cap C, cap D <: {C}, cap E <: {D}, cap F >: {C}] =
  val d1: D = ???
  val d2: CapSet^{D} = d1
  val d3: D = d2
  val e1: E = ???
  val e2: CapSet^{E} = e1
  val e3: E = e2
  val d4: D = e1
  val c1: C = d1
  val c2: C = e1
  val f1: F = c1
  val d_e_f1: CapSet^{D,E,F} = d1
  val d_e_f2: CapSet^{D,E,F} = e1
  val d_e_f3: CapSet^{D,E,F} = f1
  val f2: F = d_e_f1
  val c3: C = d_e_f1 // error
  val c4: C = f1     // error
  val e4: E = f1     // error
  val e5: E = d1     // error
  val c5: CapSet^{C} = e1


trait A[+T]

trait B[-C]

def testCong[cap C, cap D] =
  val a: A[C] = ???
  val b: A[{C}] = a
  val c: A[{D}] = a // error
  val d: A[{C,D}] = a
  val e: A[C] = d // error
  val f: B[C] = ???
  val g: B[{C}] = f
  val h: B[C] = g
  val i: B[{C,D}] = h // error
  val j: B[C] = i
