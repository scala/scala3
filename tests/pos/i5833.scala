object Dependent{
  def x: given (i: Int) => Int = ???
  def y: given (i: Int) => Int = x
}
object Independent{
  def x: given Int => Int = ???
  def y: given Int => Int = x
}
object NarrowDependent{
  def x: given Int => Int = ???
  def y: given (i: Int) => Int = x
}
object WidenDependent{
  def x: given (i: Int) => Int = ???
  def y: given Int => Int = x
}