// scalajs: --skip

/** Test the @throws annotation */
import java.io.IOException

object TestThrows {

  abstract class Foo {

    @throws(classOf[IOException])
    def read(): Int

    @throws(classOf[ClassCastException])
    @throws(classOf[IOException])
    def readWith2(): Int

    @throws(classOf[IOException])
    @Deprecated
    @throws(classOf[NullPointerException])
    def readMixed(): Int

    @Deprecated
    @throws(classOf[IOException])
    @throws(classOf[NullPointerException])
    def readMixed2(): Int

    @Deprecated
    def readNoEx(): Int
  }

  class ThrowsCtor @throws(classOf[IOException]) () {
    def value: Int = 0
  }

  def checkConstructor(cls: Class[_]): Unit = {
    val name = cls.getSimpleName
    val ctor = cls.getConstructor()
    println(name + " throws: " + ctor.getExceptionTypes.mkString("", ", ", ""))
    val annots = ctor.getDeclaredAnnotations.map(_.annotationType)
    val annotStr = annots.mkString("", ", ", "")
    println(name + " annotations:" + (if annotStr.isEmpty then "" else " " + annotStr))
  }

  def checkMethod(cls: Class[_], name: String): Unit = {
    val method = cls.getMethod(name)
    println(name + " throws: " + method.getExceptionTypes.mkString("", ", ", ""))
    val annots = method.getDeclaredAnnotations.map(_.annotationType)
    println(name + " annotations: " + annots.mkString("", ", ", ""))
  }

  def run(cls: Class[_]): Unit = {
    checkMethod(cls, "read")
    checkMethod(cls, "readWith2")
    checkMethod(cls, "readMixed")
    checkMethod(cls, "readMixed2")
    checkMethod(cls, "readNoEx")
  }
}

/** Test the top-level mirror that is has the annotations. */
object TL {

  @throws(classOf[IOException])
  def read(): Int = 0

  @throws(classOf[ClassCastException])
  @throws(classOf[IOException])
  def readWith2(): Int = 0

  @throws(classOf[IOException])
  @Deprecated
  @throws(classOf[NullPointerException])
  def readMixed(): Int = 0

  @Deprecated
  @throws(classOf[IOException])
  @throws(classOf[NullPointerException])
  def readMixed2(): Int = 0

  @Deprecated
  def readNoEx(): Int = 0
}

object Test {
  def main(args: Array[String]): Unit = {
    TestThrows.run(classOf[TestThrows.Foo])
    TestThrows.checkConstructor(classOf[TestThrows.ThrowsCtor])
    println("Testing mirror class")
    TestThrows.run(Class.forName("TL"))
  }
}
