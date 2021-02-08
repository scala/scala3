class X(x : Int)

object Y {
  val a = new X
  import a.*
  implicit val b : Int = 1
  implicit val c = 2
}
