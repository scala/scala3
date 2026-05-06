class C[T]:
  def f1[A](a: A | Null): A | Null = ???
  def f2[A](a: A): A = ???
  def g1(a: T | Null): T | Null = ???
  def g2(a: T): T = ???
  def i(a: Int | Null): Int | Null = ???

object Test:

  def printGenericSignature(m: java.lang.reflect.Method): Unit =
    val tpe = m.getGenericParameterTypes().map(_.getTypeName).mkString(", ")
    val ret = m.getGenericReturnType().getTypeName
    println(s"${m.getName}($tpe): $ret")
    
  def main(args: Array[String]): Unit =
    val c = classOf[C[_]]
    printGenericSignature(c.getDeclaredMethod("f1", classOf[Object]))
    printGenericSignature(c.getDeclaredMethod("f2", classOf[Object]))
    printGenericSignature(c.getDeclaredMethod("g1", classOf[Object]))
    printGenericSignature(c.getDeclaredMethod("g2", classOf[Object]))
    printGenericSignature(c.getDeclaredMethod("i", classOf[Object]))