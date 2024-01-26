package dotty.tools.backend.jvm

// painful to do Java reflection stuff without this
import scala.language.unsafeNulls

import org.junit.Assert.assertEquals
import org.junit.Test

import java.lang.reflect.Member

class ClassfileParserTest {
  @Test
  def noConstantPoolLag(): Unit = {
    def constNames(ms: List[Member]) = ms.collect {
      case f if f.getName.startsWith("CONSTANT_") => f.getName
    }.sorted

    val toDotc = Map(
      "CONSTANT_INTERFACE_METHODREF" -> "CONSTANT_INTFMETHODREF",
      "CONSTANT_INVOKE_DYNAMIC" -> "CONSTANT_INVOKEDYNAMIC",
      "CONSTANT_METHOD_HANDLE" -> "CONSTANT_METHODHANDLE",
      "CONSTANT_METHOD_TYPE" -> "CONSTANT_METHODTYPE",
      "CONSTANT_NAME_AND_TYPE" -> "CONSTANT_NAMEANDTYPE",
    ).withDefault(x => x)

    val asmConsts = constNames(Class.forName("scala.tools.asm.Symbol").getDeclaredFields.toList)
      .map(_.stripSuffix("_TAG"))
      .map(toDotc)
      .::("CONSTANT_UNICODE")
      .sorted
    // in the Scala 2 version of this test, we also use Java reflection to get the constant
    // names out of ClassfileConstants. in Dotty, the constants are `inline val`s, invisible
    // to Java reflection, so we hardcode them here
    assertEquals(asmConsts, List(
      // do not add to this list without also making the corresponding change
      // in ClassfileConstants! that would defeat the purpose of the test
      "CONSTANT_CLASS",
      "CONSTANT_DOUBLE",
      "CONSTANT_DYNAMIC",
      "CONSTANT_FIELDREF",
      "CONSTANT_FLOAT",
      "CONSTANT_INTEGER",
      "CONSTANT_INTFMETHODREF",
      "CONSTANT_INVOKEDYNAMIC",
      "CONSTANT_LONG",
      "CONSTANT_METHODHANDLE",
      "CONSTANT_METHODREF",
      "CONSTANT_METHODTYPE",
      "CONSTANT_MODULE",
      "CONSTANT_NAMEANDTYPE",
      "CONSTANT_PACKAGE",
      "CONSTANT_STRING",
      "CONSTANT_UNICODE",
      "CONSTANT_UTF8",
    ))
  }
}
