trait Parent {
  type Child
}

object Demo {
  def params(arr: Int): Int = ???

  def parametersOf[Parent, T]()(using m: String): Int = 0

  def combineInternal(using p: Parent): Int = {
    //this implicit needs to be available
    implicit val s: String = ""

    //parameter needs to be named
    params(arr = parametersOf[Parent, p.Child]) // error: method parametersOf must be called with () argument
  }
}
