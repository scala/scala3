import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

def test() =
  val external = File()
  class Convert:
    def apply[C^, D^ <: {C}, E^ >: {C} <: {C, external}](
        xs: List[File^{C, external}],
        ys: List[File^{D, external}])(
        zs: List[File^{E, external}]): List[File^{E, external}] = zs
  val x = File()
  val files1: List[File^{x, external}] = List(x)
  val files2: List[File^{x, external}] = List(x)
  val files3: List[File^{x, external}] = List(x)
  val _ : List[File^{x, external}] =
    Convert()[{x}, {x}, {x, external}](files1, files2)(files3)
