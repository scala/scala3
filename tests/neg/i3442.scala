class Test {
  // Any
  override def getClass(): Class[_] = ??? // error
  override def ==(that: Any): Boolean = ??? // error
  override def != (that: Any): Boolean = ??? // error
  override def ##(): Int = ??? // error
  override def isInstanceOf[T0]: Boolean = ??? // error
  override def asInstanceOf[T0]: T0 = ??? // error

  // AnyRef
  override def eq(that: AnyRef): Boolean = ??? // error
  override def ne(that: AnyRef): Boolean = ??? // error
  override def notify(): Unit = ??? // error
  override def notifyAll(): Unit = ??? // error
  override def wait(): Unit = ??? // error
  override def wait(timeout: Long, nanos: Int): Unit = ??? // error
  override def wait(timeout: Long): Unit = ??? // error
}
