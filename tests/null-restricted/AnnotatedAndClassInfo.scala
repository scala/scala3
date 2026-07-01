// Test case for AnnotatedType and ClassInfo handling in jsig
// jsig matches: case AnnotatedType(atp, _) and case ci: ClassInfo

import scala.annotation.{varargs}
import scala.specialized

class AnnotatedAndClassInfoTest:
  // Specialized type parameter
  class Specialized[@specialized T](val value: T)

  def specMethod(): Specialized[Int] = Specialized(42)

  def specMethodParam(s: Specialized[Int]): Unit = ()

  // Variable annotations
  @volatile var mutableValue: Int = 0

  def getMutable: Int = mutableValue

  def setMutable(v: Int): Unit = { mutableValue = v }

  // Field annotations
  @transient var transientField: String = "test"

  // Method with annotated return
  @inline
  def inlineMethod(x: Int): Int = x + 1

  // Varargs method
  @varargs
  def varArgsMethod(xs: Int*): Int = xs.sum

  // Generic class with annotations
  @deprecated("Old class", "1.0")
  class OldGeneric[T](val value: T):
    def getValue: T = value

  def useOldGeneric(): OldGeneric[Int] =
    OldGeneric(42)

  // Class with annotated parameter
  class Annotated(@volatile var field: String)

  def useAnnotatedClass(): Annotated =
    Annotated("test")

  // Nested annotated types
  def annotatedInGeneric(): List[String] =
    List("a", "b")

  // Annotation with arguments
  def methodWithAnnotation(x: Int): String = x.toString

  // Class extending generic class
  class StringList extends scala.collection.mutable.ListBuffer[String]:
    def appendAll(xs: String*): Unit = super.addAll(xs)

  def stringListMethod(): StringList =
    StringList()

  // Class with multiple inheritance
  trait Readable:
    def read(): String

  trait Writable:
    def write(s: String): Unit

  class ReadWrite extends Readable with Writable:
    def read(): String = "read"
    def write(s: String): Unit = ()

  def readWriteMethod: ReadWrite = ReadWrite()

class Main:
  def main(args: Array[String]): Unit = {
    val test = new AnnotatedAndClassInfoTest

    val rw = test.readWriteMethod

    val read = rw.read()
    rw.write(read)
  }
