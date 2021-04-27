sealed trait Foo {
  val index: Int
}

case class BasicFoo(index: Int) extends Foo

class NonExhaustive {
  case class FancyFoo(index: Int) extends Foo

  def convert(foos: Vector[Foo]): Vector[Int] = {
    foos.foldLeft(Vector.empty[Int]) {
      case (acc, basic: BasicFoo) => acc :+ basic.index
      case (acc, fancy: FancyFoo) => acc :+ fancy.index
    }
  }
}

@main
def Test = {
  val a = new NonExhaustive
  val b = new NonExhaustive

  val fa: Foo = a.FancyFoo(3)
  val fb: Foo = b.FancyFoo(4)

  a.convert(Vector(fa, fb))
}
