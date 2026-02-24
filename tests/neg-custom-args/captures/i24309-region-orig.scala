import language.experimental.captureChecking
import caps.*
class Ref(@caps.unsafe.untrackedCaptures private var x: Int):
  def get = x
  def set(y: Int) = x = y
trait Region extends SharedCapability:
  r: Region^ =>
    type R^
    def alloc(x: Int): Ref^{R} = Ref(x)
    def subregion[T](f: (Region { type R >: r.R }) => T): T =
      class R2 extends Region:
        type R = r.R
      f(new R2)
def withRegion[T](f: Region => T): T = f(new Region {})
@main def main() =
  withRegion: r1 =>
    val x = r1.subregion[Ref^{r1.R}]: r2 =>
      var a: Ref^{r1.R} = r1.alloc(0)
      var b: Ref^{r2.R} = r2.alloc(0)
      val c: Ref^{r1.R} = b     // error, limitation
      a                         // error, limitation
    0
