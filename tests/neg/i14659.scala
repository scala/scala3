trait Foo {
  opaque type Out = Int
  def out1: Out
  def out2: Out = out1
}

object Bar extends Foo {
  override opaque type Out = String // error
  override def out1 = "abc"
}

@main def run() =
  val x = Bar.out2