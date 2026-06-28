//> using options -Ycheck-termination -Yretain-trees

class C {
  import scala.annotation.terminates

  trait T: // error
    def f(l: List[Int]): Int = 0

  case class CC(name: String) extends T:
    override def f(l: List[Int]): Int = f(0 :: l)

  @terminates
  def g(a: T): Int = a.f(Nil)


}


