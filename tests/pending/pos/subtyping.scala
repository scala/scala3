object test {

  class B
  class C

  def tag[T](x: T): String & T = ???

  val x: Int & String = tag(0)

}


