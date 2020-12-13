
trait Show[O]:
  extension (o: O)
    def show: String

class Box[A]
class Foo

object test:

  given box[A](using Show[A]): Show[Box[A]] = _.toString
  given foo: Show[Foo] = _.toString

  def run(s: Box[Box[Foo]]): Unit =
    val x = summon[Show[Box[Box[Foo]]]]
    x.show(s)
    val r: String = box.show(s)
    println(s"step: ${box[Box[Foo]].show(s)}")
    println(s"step: ${s.show}")
