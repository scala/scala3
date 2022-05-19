import scala.language.unsafeJavaReturn

val s = "foo"
val methods: Array[java.lang.reflect.Method] = s.getClass.getMethods
