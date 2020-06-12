// Tests variance checking on default methods
import reflect.ClassTag

class Foo[+A: ClassTag](x: A) {

  private[this] val elems: Array[A] = Array(x)

  def f[B](x: Array[B] = elems): Array[B] = x // error: Found: (Foo.this.elems : Array[A])  Required: Array[B]

}
