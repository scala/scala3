final class Outer {

 

  sealed trait Inner

 

  final case class Inner1(foo: Int) extends Inner

 

  val inner: Outer#Inner = Inner1(0)

 

  def bar = inner match {

    case Inner1(i) => i

  }

}

 

object Test {

  def main(args: Array[String]): Unit = {
    val s = (new Outer).bar
    assert(s == 0)
  }

}

