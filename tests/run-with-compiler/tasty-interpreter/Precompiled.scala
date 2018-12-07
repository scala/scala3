
trait Bar {
  def meth(): Int
}

object Precompiled {
  val staticVal  = 55
  def staticMeth = 66
  // Todo
  // def staticMeth1() = 66
  // def staticMeth2(arg: Int) = arg
  // def staticMeth3(arg: Bar): Int = arg.meth()

  override def toString() = "precompiledModule"
}