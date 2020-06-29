class Foo

trait GenericTrait[A] {
  def bar: A
}

trait ConcreteTrait extends GenericTrait[Foo] {
  val bar: Foo = new Foo
}

class ConcreteClass extends ConcreteTrait

object Test {
  def main(args: Array[String]): Unit = {
    val obj = new ConcreteClass
    assert(obj.bar != null)
  }
}
