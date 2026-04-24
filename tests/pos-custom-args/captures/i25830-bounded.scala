import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

// Bounded capture polymorphism: retained refs in type-parameter bounds
// survive cleanup when they refer to parameters of the inferred lambda type.
def testFlat() =
  val f = { [C^, D^ <: {C}] => (xs: List[File^{D}]) => xs }
  val a = File()
  val _ : List[File^{a}] = f[{a}, {a}](List[File^{a}](a))
