
class Bar extends IFace {
  def meth(): Int = 60
  def methA(x: Int): Int = x
}

trait IFace {
  def meth(): Int
  def methA(x: Int): Int
}

object Precompiled {
  def staticMeth = 55
  val staticVal  = 56

  // Todo
  def staticMeth1() = 57
  def staticMeth2(arg: Int) = arg
  def staticMeth3(arg: Object): Int = 59
  def staticMeth4(arg: IFace): Int = arg.meth()
  def staticMeth5(arg: IFace, x: Int): Int = arg.methA(x)

  override def toString() = "precompiledModule"
}