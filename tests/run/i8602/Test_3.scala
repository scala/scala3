// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit =
    val child = new ChildClass_2()
    assert(child.foo == 5)
    assert(child.getFoo() == 5)
    val parent: ParentTrait = child
    assert(parent.foo == 5)
}
