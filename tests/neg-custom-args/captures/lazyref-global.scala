
class CC
type Cap = CC^

class LazyRef[T](val elem: () => T):
  def get: () => T = elem  // error: separation failure
  def map[U](f: T => U): LazyRef[U]^{f, this} =
    new LazyRef(() => f(elem())) // error: separation failure

def map[A, B](ref: LazyRef[A]^, f: A => B): LazyRef[B]^{f, ref} =
  new LazyRef(() => f(ref.elem()))

def mapc[A, B]: (ref: LazyRef[A]^, f: A => B) -> LazyRef[B]^{f, ref} =
  (ref1, f1) => map[A, B](ref1, f1)

val cap1: Cap = CC()
val cap2: Cap = CC()

def test() =
  def f(x: Int) = if cap1 == cap1 then x else 0
  def g(x: Int) = if cap2 == cap2 then x else 0
  val ref1 = LazyRef(() => f(0))
  val ref1c: LazyRef[Int] = ref1 // error
  val ref2 = map(ref1, g)
  val ref2c: LazyRef[Int]^{cap2} = ref2 // error
  val ref3 = ref1.map(g)
  val ref3c: LazyRef[Int]^{ref1} = ref3 // error
  val ref4 = (
      if cap1 == cap2
      then ref1
      else ref2)
    .map(g) // error: separation failure
  val ref4c: LazyRef[Int]^{cap1} = ref4 // error
