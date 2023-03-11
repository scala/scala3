class Foo[+T]
class Elem:
  def one(a: Elem, x: Foo[a.type]): Int = x.ext
  def two(x: Foo[Elem.this.type]): Int  = x.ext
object Elem:
  extension (x: Foo[Elem]) def ext: Int = 1
