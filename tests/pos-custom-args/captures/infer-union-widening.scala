import language.experimental.captureChecking

// Inferring the capture set arguments of `p` needs the constraints
//   A <: {Y, B}, A <: {z}, B <: {D}, z <: {A}
// to be solved as A = {z}, B = {D}. Adding `z` to the union {Y, B} must
// widen `z` to its underlying set {D, Y} against the whole union: `Y` is
// accounted for by the first operand and `D` enters `B`.
def test1[D^, Y^](z: () ->{D, Y} Unit): Unit =
  def p[A^, B^](k1: (() ->{A} Unit) => () ->{Y, B} Unit,
                k2: (() ->{A} Unit) => Unit,
                k3: (() ->{B} Unit) => Unit,
                f:  () ->{A} Unit): Unit = ()
  p(g => g, (g: () ->{z} Unit) => (), (g: () ->{D} Unit) => (), () => z())
  p[{z}, {D}](g => g, (g: () ->{z} Unit) => (), (g: () ->{D} Unit) => (), () => z())  // the same solution written out
