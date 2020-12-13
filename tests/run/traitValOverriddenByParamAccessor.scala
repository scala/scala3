trait Foo {
  val someField: Int = 5
}

class SimpleFoo extends Foo

class Bar(override val someField: Int) extends Foo

object Test {
  def main(args: Array[String]): Unit = {
    val simpleFoo = new SimpleFoo
    assert(simpleFoo.someField == 5)

    val bar = new Bar(44)
    assert(bar.someField == 44)
  }
}
