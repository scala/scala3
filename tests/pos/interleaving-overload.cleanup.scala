// A minimisation of interleaving-overload
// Used while developing the tvar/tl clearnup in normalizedCompatible
class B[U]
class Test():
  def fn[T]: [U] => Int => B[U] = [U] => (x: Int) => new B[U]()
  def test(): Unit =
    fn(1)
    fn(2)
    ()
