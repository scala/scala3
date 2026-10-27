//> using options -source future

import java.lang.invoke.{MethodHandle, VarHandle}

class C:
  // the cast's expected type sets the result type (deprecated, ignored under -source future)
  def dep1(mh: MethodHandle, c: AnyRef): String = mh.invokeExact(c).asInstanceOf[String] // warn
  def dep2(mh: MethodHandle, c: AnyRef): Int = mh.invokeExact(c).asInstanceOf[Int] // warn
  // the cast's expected type is an uninstantiated type variable
  def dep3(vh: VarHandle) = Option(vh.get().asInstanceOf[String]) // warn

  // the call returns Object
  def warn(mh: MethodHandle, vh: VarHandle, c: AnyRef): Unit =
    val a = mh.invokeExact(c).asInstanceOf[Int] // warn
    val b = mh.invoke(c).asInstanceOf[String] // warn
    val d: Any = vh.getVolatile(c).asInstanceOf[Long] // warn
    val e = Option(mh.invokeExact(c).asInstanceOf[Int]) // warn
    val f = List(mh.invokeExact(c).asInstanceOf[Int], 1L) // warn

  def noWarn[T](mh: MethodHandle, vh: VarHandle, c: AnyRef): Unit =
    val a = (mh.invokeExact(c): Int)
    val b = mh.invoke(c).asInstanceOf[AnyRef]
    val d = mh.invoke(c).asInstanceOf[T]
    val e = (mh.invoke(c): Any).asInstanceOf[Int]
    val f = vh.compareAndSet(c, 1, 2).asInstanceOf[Boolean]
    val g = Option(mh.invokeExact(c).asInstanceOf[T])
