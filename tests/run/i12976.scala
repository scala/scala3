// scalajs: --skip

case class A(s: String)

class B {
  def b1[X](str: String): String = str

  def b2[X](str: String): X = null.asInstanceOf[X]
}

object Test {

  def main(args: Array[String]): Unit = {
    val a = A("aaa")
    val b = new B

    // no error
    a match {
      case A(s) =>
        b.b1(s)
    }

    // no error if add explicit type param
    a match {
      case A(s) =>
        b.b2[Boolean](s)
    }

    // scala.MatchError: A(aaa)
    try
      a match {
        case A(s) =>
          b.b2(s)
      }
      assert(false)
    catch case ex: NullPointerException =>
      () // OK
  }

}
