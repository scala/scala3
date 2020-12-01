class A {
  @scala.beans.BeanProperty val x = 4
  @scala.beans.BooleanBeanProperty val y = true
  @scala.beans.BeanProperty var mutableOneWithLongName = "some text"

  @scala.beans.BeanProperty
  @beans.LibraryAnnotation_1
  val retainingAnnotation = 5
}
