import scala.annotation.varargs

abstract class VarargAbstractClass[T] {
  @varargs
  def x(v: Int*): Int

  @varargs
  def y(v: String*): Int

  @varargs
  def z(v: T*): Int

  @varargs
  def generic[E](e: E*): Unit = ()

  @varargs
  def genericBounded[E <: Comparable[E] & Serializable](e: E*): Unit = ()
}

class VarargImplClass extends VarargAbstractClass[String] {

  override def x(v: Int*): Int = v.length
  override def y(v: String*): Int = v.length
  override def z(v: String*): Int = v.length
}

class VarargClassBounded[B <: Comparable[B]] {
  @varargs
  def v1(v: B*): Unit = ()

  @varargs
  def v2(first: B, more: B*): B = first
}
