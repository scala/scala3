import annotation.static
object T {
  @static val foo = 10 // error: needs companion class
}
