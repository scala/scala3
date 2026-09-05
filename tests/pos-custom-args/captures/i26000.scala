import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

@main def test =
  // The type of the lambda is captured in the inferred type argument of `Box`
  // and in the inferred type of `f`. The `^{C}` capture sets and the bound of
  // `C` must be preserved there.
  val f = Box([C^] => (f: File^{C}) => (h: File^{C}) => (g: File^{C}) => { println(h); (f, h) })

  val x: File^ = File()
  val (r1, r2) = f.value[{x}](x)(x)(x)
  val _: File^{x} = r1
  val _: File^{x} = r2

  // The same, one level further out: the inferred type of `g` is derived from
  // the inferred type of a local definition. Note that `g.value` cannot be
  // instantiated with an external capture set, since the capture set parameter's
  // upper bound in the result type of `g` is the result's `fresh` capability;
  // this is the same behavior as for an explicitly declared result type.
  def g =
    val b = Box([C^] => (xs: List[File^{C}]) => xs)
    b
  val files: List[File^{x}] = List(x)
  val r3 = g.value[{}](List(File()))
  val _: List[File] = r3

  // A bounded capset parameter in an inferred type argument.
  val io: File^ = File()
  val bounded = Box([C^ <: {io}] => (f: File^{C}) => f)
  val r4 = bounded.value[{io}](io)
  val _: File^{io} = r4
