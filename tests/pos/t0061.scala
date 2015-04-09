object O {

  class testClass ;

  case class testA() extends testClass ;

  def ga( x:testClass ) = x match {
      case testA() => ()
  }
}
