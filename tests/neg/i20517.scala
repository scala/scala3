import NamedTuple.From

case class Foo[+T](elem: T)

trait Base[M[_]]:
  def dep(foo: Foo[Any]): M[foo.type]

class SubAny extends Base[From]:
  def dep(foo: Foo[Any]): From[foo.type] = (elem = "") // error

object Test:
  @main def run =
    val f: Foo[Int] = Foo(elem = 1)
    val b: Base[From] = SubAny()
    val nt: (elem: Int) = b.dep(f)
    val x: Int = nt.elem // was ClassCastException