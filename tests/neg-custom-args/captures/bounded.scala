// To be revisited
class CC
type Cap = {*} CC

def test(c: Cap) =
  class B[X <: {c} Object](x: X):
    def elem = x
    def lateElem = () => x

  def f(x: Int): Int = if c == c then x else 0
  val b = new B(f)
  val r1 = b.elem
  val r1c: {c} Int -> Int = r1
  val r2 = b.lateElem
  val r2c: () -> {c} Int -> Int = r2 // error

def test2(c: Cap) =
  class B[X <: {*} Any](x: X):
    def elem = x
    def lateElem = () => x

  def f(x: Int): Int = if c == c then x else 0
  val b = new B(f)
  val r1 = b.elem
  val r1c: {c} Int -> Int = r1
  val r2 = b.lateElem
  val r2c: () -> {c} Int -> Int = r2 // was error now OK