object Test {

  def hasGenericSignature(cls: Class[_], methName: String): Boolean = {
    cls.getDeclaredMethods().find(_.getName.contains(methName)) match {
      case None => throw new NoSuchMethodError(s"No $methName in ${cls.getName}")
      case Some(meth) => meth.getTypeParameters.nonEmpty
    }
  }

  def checkHasGenericSignature(cls: Class[_], methName: String): Unit =
    assert(hasGenericSignature(cls, methName))

  def checkDoesntHaveGenericSignature(cls: Class[_], methName: String): Unit =
    assert(!hasGenericSignature(cls, methName))

  def main(args: Array[String]): Unit = {

    checkHasGenericSignature(classOf[TopLevelClass], "meth")
    checkHasGenericSignature(classOf[AbstractTopLevelClass], "meth")
    checkHasGenericSignature(classOf[TopLevelClass#InsideClass], "meth")
    checkHasGenericSignature(classOf[TopLevelClass#AbstractInsideClass], "meth")
    checkDoesntHaveGenericSignature(new TopLevelClass().localClass, "meth")
    checkDoesntHaveGenericSignature(new TopLevelClass().otherLocalClass, "meth")

    checkHasGenericSignature(TopLevelObject.getClass, "meth")
    checkHasGenericSignature(classOf[TopLevelObject.InsideObject], "meth")
    checkHasGenericSignature(classOf[TopLevelObject.AbstractInsideObject], "meth")
    checkDoesntHaveGenericSignature(TopLevelObject.localClass, "meth")
    checkDoesntHaveGenericSignature(TopLevelObject.otherLocalClass, "meth")

    println("OK")
  }
}

object TopLevelObject {
  def meth[T](x: T): T = x

  def localObject: Class[_] = {
    object LocalObject {
      def meth[T](x: T): T = x
    }
    LocalObject.getClass
  }

  def localClass: Class[_] = {
    class LocalClass {
      def meth[T](x: T): T = x
    }
    classOf[LocalClass]
  }

  val otherLocalClass: Class[_] = {
    class LocalClass {
      def meth[T](x: T): T = x
    }
    classOf[LocalClass]
  }

  class InsideObject {
    def meth[T](x: T): T = x
  }

  abstract class AbstractInsideObject {
    def meth[T](x: T): T = x
  }
}

class TopLevelClass {

  def meth[T](x: T): T = x

  def localClass: Class[_] = {
    class LocalClass {
      def meth[T](x: T): T = x
    }
    classOf[LocalClass]
  }

  val otherLocalClass: Class[_] = {
    class LocalClass {
      def meth[T](x: T): T = x
    }
    classOf[LocalClass]
  }

  class InsideClass {
    def meth[T](x: T): T = x
  }

  abstract class AbstractInsideClass {
    def meth[T](x: T): T = x
  }
}

abstract class AbstractTopLevelClass {
  def meth[T](x: T): T = x
}

