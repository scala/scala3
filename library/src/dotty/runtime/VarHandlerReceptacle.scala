package dotty.runtime

import java.lang.invoke.MethodHandles

import scala.reflect.ClassTag

class VarHandlerReceptacle(lookup: MethodHandles.Lookup) extends RuntimeHandleReceptacle {

  override def get[U <: AnyRef : ClassTag](ref: Object, clazz: Class[_], varName: String)(implicit tag: ClassTag[U]): U  = lookup.findVarHandle(clazz, varName, tag.runtimeClass).get(ref.asInstanceOf[Object]).asInstanceOf[U]

  override def put[U <: AnyRef : ClassTag](ref: Object, clazz: Class[_], varName: String, value: U)(implicit tag: ClassTag[U]): Unit = lookup.findVarHandle(clazz, varName, tag.runtimeClass).set(ref.asInstanceOf[Object], value.asInstanceOf[Object]).asInstanceOf[U]

  override def compareAndSwapLong(ref: Object, clazz: Class[_], varName: String, expected: Long, value: Long): Boolean =  lookup.findVarHandle(clazz, varName, classOf[Long]).compareAndSet(ref, Long.box(expected), Long.box(value))

  override def getLongVolatile(ref: Object, clazz: Class[_], varName: String): Long = ???
}
