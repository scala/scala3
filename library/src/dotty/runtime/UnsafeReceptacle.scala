package dotty.runtime

import sun.misc.Unsafe

import scala.reflect.ClassTag


class UnsafeReceptacle(runtimeReference: Unsafe) extends RuntimeHandleReceptacle {

  override def get[U <: AnyRef : ClassTag](ref: Object, clazz: Class[_], varName: String)(implicit tag: ClassTag[U]): U = runtimeReference.getObject(ref, getOffset(clazz, varName)).asInstanceOf[U]
  override def getLongVolatile(ref: Object, clazz: Class[_], varName: String): Long = runtimeReference.getLongVolatile(ref, getOffset(clazz, varName))

  override def put[U : ClassTag](ref: Object, clazz: Class[_], varName: String, value: U)(implicit tag: ClassTag[U]): Unit = runtimeReference.putObject(ref, getOffset(clazz, varName), value)

  override def compareAndSwapLong(ref: Object, clazz: Class[_], varName: String, expected: Long, value: Long): Boolean = runtimeReference.compareAndSwapLong(ref, getOffset(clazz, varName), expected, value)

  private def getOffset(clazz: Class[_], varName: String): Long = runtimeReference.objectFieldOffset(clazz.getDeclaredField(varName))


}