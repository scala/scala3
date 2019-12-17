trait A extends Any {
    case class B() // error
    val x = {
      case class C() // error
      1
    }
    def f = {
      case class C() // ok
      1
    }
}