
class Foo {
  @annot type A = Int
  @annot val a: Int = ???
  val b: Int @annot = ???
  def c(x: Int) = (x : @annot)
}

class annot extends scala.annotation.Annotation
