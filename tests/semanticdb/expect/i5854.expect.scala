package i5854

class B/*<-i5854::B#*/ {
  val a/*<-i5854::B#a.*/: String/*->scala::Predef.String#*/ = (((1: Any/*->scala::Any#*/): b/*->i5854::B#b.*/.A): Nothing/*->scala::Nothing#*/): String/*->scala::Predef.String#*/
  val b/*<-i5854::B#b.*/: { type A/*<-local0*/ >: Any/*->scala::Any#*/ <: Nothing/*->scala::Nothing#*/ } = loop/*->i5854::B#loop().*/() // error
  def loop/*<-i5854::B#loop().*/(): Nothing/*->scala::Nothing#*/ = loop/*->i5854::B#loop().*/()
}
