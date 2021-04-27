class A {
  inline def getInline: String = {
    class Local {
      private val y: Int = 1
    }
    val a = scala.reflect.classTag[Integer]
    println(new Local)
    val x = 1
    x.toString
  }
}
