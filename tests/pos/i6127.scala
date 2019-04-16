import reflect.ClassTag
class Co[+S]
object Co {
  def empty[X: ClassTag]: Co[X] = ???
}
class Contra[-S]
object Contra {
  def empty[X: ClassTag]: Contra[X] = ???
}
class Foo[+FT](x: FT) {
  def fooArray: Foo[Array[String]] = new Foo(Array.empty)
  val y1: Array[String] = Array.empty
  def fooCo: Foo[Co[String]] = new Foo(Co.empty)
  val y2: Co[String] = Co.empty
  def fooContra: Foo[Contra[String]] = new Foo(Contra.empty)
  val y3: Contra[String] = Contra.empty
}

