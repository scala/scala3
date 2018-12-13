
trait Bar {
  def meth(): Int
}

object Precompiled {
  def staticMeth = 55
  val staticVal  = 56

  // Todo
  def staticMeth1() = 57
  def staticMeth2(arg: Int) = arg
  def staticMeth3(arg: Object): Int = 59
  def staticMeth4(arg: Bar): Int = arg.meth()

  override def toString() = "precompiledModule"
}