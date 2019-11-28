import scala.reflect.ClassTag
class Foo[+A: ClassTag](x: A) {

  private[this] val elems: Array[A] = Array(x)

  def f[B](x: Array[B] = elems): Array[B] = x // error (1) should give a variance error here or ...

}