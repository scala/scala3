import language.future // sepchecks on
import caps.consume

class Cap extends caps.Capability

class LazyRef[T](val elem: () => T):
  val get: () ->{elem} T = elem
  def map[U](f: T => U): LazyRef[U]^{f, this} =
    new LazyRef(() => f(elem()))

def map[A, B](ref: LazyRef[A]^, f: A => B): LazyRef[B]^{f, ref} =
  new LazyRef(() => f(ref.elem()))

def mapc[A, B]: (ref: LazyRef[A]^, f: A => B) => LazyRef[B]^{f, ref} =
  (ref1, f1) => map[A, B](ref1, f1)

def test(@consume cap1: Cap, @consume cap2: Cap) =
  def f(x: Int) = if cap1 == cap1 then x else 0
  def g(x: Int) = if cap2 == cap2 then x else 0
  val ref1 = LazyRef(() => f(0))
  val ref1c: LazyRef[Int]^{cap1} = ref1
  val ref2 = map(ref1, g)
  val ref2c: LazyRef[Int]^{cap2, ref1} = ref2
  val ref3 = ref1.map(g)
  val ref3c: LazyRef[Int]^{cap2, ref1} = ref3
  val ref4 = (if cap1 == cap2 then ref1 else ref2).map(g)
  val ref4c: LazyRef[Int]^{cap1, cap2} = ref4
