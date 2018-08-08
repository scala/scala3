class Foo {

  def justdoit (f : Either[Int,String]) : String = {
    f match {
      case Left(i) => i.toString
      case Right(s) => s
    }

  }
}
