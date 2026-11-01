package p
import language.experimental.captureChecking
import caps.*

class File

def test(a: File^, b: File^, c: Boolean) =
  val pf = [C^ <: {a}] => (f: File^{C}) => f
  pf[{b}](b) // error

  val l1 = pf
  l1[{b}](b) // error

  val l2: [C^] -> (f: File^{C}) -> File^{f} = pf // error

  val pf2 = [C^ <: {b}] => (f: File^{C}) => f
  val l3 = if c then pf else pf2
  l3[{b}](b) // error // error

// bounds are contravariant, and `caps.any` is the top capability in them
def widening(x: File^, foo: File^) =
  val g: [C^, D^ <: {C}] -> (g: File^{D}) -> File^{C} = [C^, D^ <: {C}] => (g: File^{D}) => (g: File^{C})
  val wide: [C^ <: {any, foo}] -> (f: File^{C}) -> Unit = [C^ <: {any, foo}] => (f: File^{C}) => ()
  val sep: [C^] -> (f: File^{C}) -> Unit = [C^] => (f: File^{C}) => ()
  val a: [C^] -> (f: File^{C}) -> Unit = wide
  val b: [C^ <: {any, foo}] -> (f: File^{C}) -> Unit = sep
  val c: [C^ <: {x}] -> (f: File^{C}) -> Unit = sep
  val d: [C^] -> (f: File^{C}) -> Unit = [C^ <: {any.rd}] => (f: File^{C}) => () // error

// bounds can refer to earlier type parameters, also in inferred types
class Box[A](val value: A)
type DP = [C^, D^ <: {C}] -> (f: File^{C}, g: File^{D}) -> File^{C}
def dependent(x: File^, y: File^, g: DP) =
  val b: Box[DP] = Box(g)
  val h = g
  h[{x}, {x}](x, x)
  h[{x}, {y}](x, y) // error
