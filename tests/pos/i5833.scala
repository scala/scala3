object Dependent{
  def x: (i: Int) ?=> Int = ???
  def y: (i: Int) ?=> Int = x
}
object Independent{
  def x: Int ?=> Int = ???
  def y: Int ?=> Int = x
}
object NarrowDependent{
  def x: Int ?=> Int = ???
  def y: (i: Int) ?=> Int = x
}
object WidenDependent{
  def x: (i: Int) ?=> Int = ???
  def y: Int ?=> Int = x
}