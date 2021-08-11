abstract class NullaryTest/*<-_empty_::NullaryTest#*/[T/*<-_empty_::NullaryTest#[T]*/, m/*<-_empty_::NullaryTest#[m]*/[s/*<-_empty_::NullaryTest#`<init>`().[m][s]*/]] {
  def nullary/*<-_empty_::NullaryTest#nullary().*/: String/*->scala::Predef.String#*/ = "a"
  val x/*<-_empty_::NullaryTest#x.*/ = nullary/*->_empty_::NullaryTest#nullary().*/

  def nullary2/*<-_empty_::NullaryTest#nullary2().*/: T/*->_empty_::NullaryTest#[T]*/
  val x2/*<-_empty_::NullaryTest#x2.*/ = nullary2/*->_empty_::NullaryTest#nullary2().*/

  def nullary3/*<-_empty_::NullaryTest#nullary3().*/: m/*->_empty_::NullaryTest#[m]*/[T/*->_empty_::NullaryTest#[T]*/]
  val x3/*<-_empty_::NullaryTest#x3.*/ = nullary3/*->_empty_::NullaryTest#nullary3().*/
}

class Concrete/*<-_empty_::Concrete#*/ extends NullaryTest/*->_empty_::NullaryTest#*/[Int/*->scala::Int#*/, List/*->scala::package.List#*/] {
  def nullary2/*<-_empty_::Concrete#nullary2().*/ = 1
  def nullary3/*<-_empty_::Concrete#nullary3().*/ = List/*->scala::package.List.*/(1,2,3)
}

object test/*<-_empty_::test.*/ {
  (new Concrete/*->_empty_::Concrete#*/).nullary2/*->_empty_::Concrete#nullary2().*/
  (new Concrete/*->_empty_::Concrete#*/).nullary3/*->_empty_::Concrete#nullary3().*/
}
