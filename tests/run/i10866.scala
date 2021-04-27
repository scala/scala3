abstract class B{
  def unapply(arg:A)={println ("Unapply from B"); true}
}

case class A()

object A extends B

object Test extends App {
  println("Test")
  A() match {
    case A() => println("done")
  }
}
