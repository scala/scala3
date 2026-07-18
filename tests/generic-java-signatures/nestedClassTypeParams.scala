class Outer[T]:
  def outerValue: T = ???

  class Middle[U]:
    def middleValue: U = ???

    class Inner[V]:
      def innerValue: V = ???
      def noShadow(x: T, y: U, z: V): (T, U, V) = (x, y, z)
      def shadowOuter[T] = outerValue
      def shadowMiddle[U] = middleValue
      def shadowInner[V] = innerValue

@main def Test(): Unit =
  val innerMethods = classOf[Outer[_]].getDeclaredClasses()
    .find(_.getName.contains("Middle")).get.getDeclaredClasses()
    .find(_.getName.contains("Inner")).get.getDeclaredMethods()

  printMethodSig(innerMethods, "noShadow")
  printMethodSig(innerMethods, "shadowOuter")
  printMethodSig(innerMethods, "shadowMiddle")
  printMethodSig(innerMethods, "shadowInner")

def printMethodSig(methods: Array[java.lang.reflect.Method], name: String): Unit =
  methods.find(_.getName.endsWith(name)).foreach { m =>
    println(s"$name: ${m.toGenericString}")
  }
