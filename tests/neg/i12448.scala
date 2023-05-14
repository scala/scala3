object Main {
  def mkArray[T <: A]: T#AType // error // error
  mkArray[Array] // was: "assertion failed: invalid prefix HKTypeLambda..."
  val x = mkArray[Array]
}
