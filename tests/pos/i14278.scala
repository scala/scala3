// https://github.com/scala/scala3/issues/14278
class Foo

extension (foo: Foo)
  def patch(arg: List[Int], arg2: Int = 0): Unit = {}
  def patch(arg: Int): Unit = patch(List(arg))
