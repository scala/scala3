package i5854

class B/*<-i5854::B#*/ {
  // Known issue: Can't lookup the symbol of `b.A`
  // we have to register the symbol of `b { type A }` to the refinementSymtab first
  // then resolve, or assign same semanticdb symbol for both
  // fake symbol for b.A, and real symbol of A in b
  val a/*<-i5854::B#a.*/: String/*->scala::Predef.String#*/ = (((1: Any/*->scala::Any#*/): b/*->i5854::B#b.*/.A): Nothing/*->scala::Nothing#*/): String/*->scala::Predef.String#*/
  val b/*<-i5854::B#b.*/: { type A/*<-local0*/ >: Any/*->scala::Any#*/ <: Nothing/*->scala::Nothing#*/ } = loop/*->i5854::B#loop().*/() // error
  def loop/*<-i5854::B#loop().*/(): Nothing/*->scala::Nothing#*/ = loop/*->i5854::B#loop().*/()
}
