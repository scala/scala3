package dotty.runtime

import java.lang.invoke.MethodHandles

import scala.reflect.ClassTag

/**
 * sun.misc.Unsafe and java.lang.invoke.VarHandle receptacle.
 * It allows Java 8-9+ compatibility without using 9-deprecated Unsafe class.
 */

trait RuntimeHandleReceptacle {


  def get[U <: AnyRef : ClassTag](ref: Object, clazz: Class[_], varName: String)(implicit tag: ClassTag[U]): U

  def getLongVolatile(ref: Object, clazz: Class[_], varName: String): Long

  def put[U <: AnyRef : ClassTag](ref: Object, clazz: Class[_], varName: String, value: U)(implicit tag: ClassTag[U]): Unit

  def compareAndSwapLong(ref: Object, clazz: Class[_], varName: String, expected: Long, value: Long): Boolean
}

object RuntimeHandleReceptacle {

  private val lookup = MethodHandles.lookup()
  private val reference: RuntimeHandleReceptacle = {
    try {
      //Java 9+
      Class.forName("java.lang.invoke.VarHandle")
      new VarHandlerReceptacle(lookup)
    } catch {
      //Java 8
      case e: ClassNotFoundException =>
        val unsafeClass = Class.forName("sun.misc.Unsafe")
        var unsafeReference: sun.misc.Unsafe = null
        for (field <- unsafeClass.getDeclaredFields) if (field.getType.isAssignableFrom(unsafeClass)) unsafeReference = field.get(null).asInstanceOf[sun.misc.Unsafe]
        new UnsafeReceptacle(unsafeReference)
    }
  }

  def apply(): RuntimeHandleReceptacle = reference
}