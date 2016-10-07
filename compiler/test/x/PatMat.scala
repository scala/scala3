package x

trait PatMat {

  trait A
  type TT <: B1
  trait B2
  trait B1 extends B
  case class B()

  def foo[T <: Any](x: TT) = x match {
    case x: B => println("!!!")
    case "hi" => ???
  }

}
