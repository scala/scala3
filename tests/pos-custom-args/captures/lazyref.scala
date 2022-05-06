@annotation.capability class Cap

class LazyRef[T](val elem: () => T):
  val get: {elem} () -> T = elem
  def map[U](f: T => U): {f, this} LazyRef[U] =
    new LazyRef(() => f(elem()))

def map[A, B](ref: {*} LazyRef[A], f: A => B): {f, ref} LazyRef[B] =
  new LazyRef(() => f(ref.elem()))

def mapc[A, B]: (ref: {*} LazyRef[A], f: A => B) => {f, ref} LazyRef[B] =
  (ref1, f1) => map[A, B](ref1, f1)

def test(cap1: Cap, cap2: Cap) =
  def f(x: Int) = if cap1 == cap1 then x else 0
  def g(x: Int) = if cap2 == cap2 then x else 0
  val ref1 = LazyRef(() => f(0))
  val ref1c: {cap1} LazyRef[Int] = ref1
  val ref2 = map(ref1, g)
  val ref2c: {cap2, ref1} LazyRef[Int] = ref2
  val ref3 = ref1.map(g)
  val ref3c: {cap2, ref1} LazyRef[Int] = ref3
  val ref4 = (if cap1 == cap2 then ref1 else ref2).map(g)
  val ref4c: {cap1, cap2} LazyRef[Int] = ref4
