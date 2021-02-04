class A:
  @scala.beans.BeanProperty val x = 4
  @scala.beans.BooleanBeanProperty val y = true
  @scala.beans.BeanProperty var mutableOneWithLongName = "some text"

  @scala.beans.BeanProperty
  @beans.LibraryAnnotation_1
  val retainingAnnotation = 5

trait T:
  @scala.beans.BeanProperty val x: Int

class T1 extends T:
  override val x = 5

class T2 extends T1:
  override val x = 10