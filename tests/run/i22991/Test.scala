
//Test java runtime reflection access to @Runtime annotations on method parameters.
object Test extends App:
  val method: java.lang.reflect.Method = classOf[Bar].getMethod("bar", classOf[String], classOf[Int], classOf[Object], classOf[Int])
  val annots: Array[Array[java.lang.annotation.Annotation]] = method.getParameterAnnotations()
  assert(annots.length == 4)
  assert(annots(0).length == 0)
  assert(annots(1).length == 1)
  assert(annots(1)(0).isInstanceOf[Foo])
  assert(annots(2).length == 1)
  assert(annots(2)(0).isInstanceOf[Foo])
  assert(annots(3).length == 0)

  val method2: java.lang.reflect.Method = classOf[Bar].getMethod("bar2", classOf[Int])
  val annots2: Array[Array[java.lang.annotation.Annotation]] = method2.getParameterAnnotations()
  assert(annots2.length == 1)
  assert(annots2(0)(0).isInstanceOf[Foo])
