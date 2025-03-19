object Main {
  def mkArray[T <: A]: T#AType // error // error
  mkArray[Array]
  val x = mkArray[Array]
}
