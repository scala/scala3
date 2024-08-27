// Justifies the need to add defn.PolyFunctionOf in simplify
// Without, the TypeVar for the U in fn's lambda
// replaces the TypeParamRef U, in simplify.
class B[U]
class Test():
  def fn[T]: [U] => Int => B[U] = [U] => (x: Int) => new B[U]()
  def test(): Unit =
    fn(1)
    fn(2)
    ()
