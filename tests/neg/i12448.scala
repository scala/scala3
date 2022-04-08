class A
object Main {
  def mkArray[T <: A]: T#AType // error // error
  mkArray[Array]
}