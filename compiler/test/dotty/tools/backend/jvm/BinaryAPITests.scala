package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm
import asm._
import asm.tree._

import scala.tools.asm.Opcodes
import scala.jdk.CollectionConverters._
import Opcodes._

class BinaryAPITests extends DottyBytecodeTest {
  import ASMConverters._

  private def privateOrProtectedOpcode = Opcodes.ACC_PRIVATE | Opcodes.ACC_PROTECTED

  private def checkPublicMethod(classNode: ClassNode, methodName: String, desc: String): Unit =
    val method = getMethod(classNode, methodName)
    assert(method.desc == desc)
    assert((method.access & privateOrProtectedOpcode) == 0)

  private def checkPrivateMethod(classNode: ClassNode, methodName: String, desc: String): Unit =
    val method = getMethod(classNode, methodName)
    assert(method.desc == desc)
    assert((method.access & Opcodes.ACC_PRIVATE) == Opcodes.ACC_PRIVATE)

  private def checkPublicField(classNode: ClassNode, fliedName: String): Unit =
    val method = getField(classNode, fliedName)
    assert((method.access & privateOrProtectedOpcode) == 0)

  private def checkPrivateField(classNode: ClassNode, fliedName: String): Unit =
    val method = getField(classNode, fliedName)
    assert((method.access & Opcodes.ACC_PRIVATE) == Opcodes.ACC_PRIVATE)

  @Test
  def binaryAPIDef(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |class C:
        |  @binaryAPI private def privateBinaryAPI: Int = 1
        |  @binaryAPI private[C] def packagePrivateBinaryAPI: Int = 1
        |  @binaryAPI protected def protectedBinaryAPI: Int = 1
        |  inline def inlined = privateBinaryAPI + packagePrivateBinaryAPI + protectedBinaryAPI
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPrivateMethod(cClass, "privateBinaryAPI", "()I")
      checkPublicMethod(cClass, "C$$inline$privateBinaryAPI", "()I")
      checkPublicMethod(cClass, "packagePrivateBinaryAPI", "()I")
      checkPublicMethod(cClass, "protectedBinaryAPI", "()I")

      // Check that the @binaryAPI annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$privateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedBinaryAPI", "()I", false),
      ))
    }
  }

  @Test
  def binaryAPIVal(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |class C:
        |  @binaryAPI private val privateBinaryAPI: Int = 1
        |  @binaryAPI private[C] val packagePrivateBinaryAPI: Int = 1
        |  @binaryAPI protected val protectedBinaryAPI: Int = 1
        |  @binaryAPI private lazy val lazyPrivateBinaryAPI: Int = 1
        |  @binaryAPI private[C] lazy val lazyPackagePrivateBinaryAPI: Int = 1
        |  @binaryAPI protected lazy val lazyProtectedBinaryAPI: Int = 1
        |  inline def inlined = privateBinaryAPI + packagePrivateBinaryAPI + protectedBinaryAPI + lazyPrivateBinaryAPI + lazyPackagePrivateBinaryAPI + lazyProtectedBinaryAPI
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPrivateField(cClass, "privateBinaryAPI")
      checkPublicMethod(cClass, "C$$inline$privateBinaryAPI", "()I")
      checkPublicMethod(cClass, "packagePrivateBinaryAPI", "()I")
      checkPublicMethod(cClass, "protectedBinaryAPI", "()I")

      checkPrivateMethod(cClass, "lazyPrivateBinaryAPI", "()I")
      checkPublicMethod(cClass, "C$$inline$lazyPrivateBinaryAPI", "()I")
      checkPublicMethod(cClass, "lazyPackagePrivateBinaryAPI", "()I")
      checkPublicMethod(cClass, "lazyProtectedBinaryAPI", "()I")

      // Check that the @binaryAPI annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$privateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$lazyPrivateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "lazyPackagePrivateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "lazyProtectedBinaryAPI", "()I", false),
      ))
    }
  }

  @Test
  def binaryAPIVar(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |class C:
        |  @binaryAPI private var privateBinaryAPI: Int = 1
        |  @binaryAPI private[C] var packagePrivateBinaryAPI: Int = 1
        |  @binaryAPI protected var protectedBinaryAPI: Int = 1
        |  inline def inlined =
        |    privateBinaryAPI = 1
        |    packagePrivateBinaryAPI = 1
        |    protectedBinaryAPI = 1
        |    privateBinaryAPI + packagePrivateBinaryAPI + protectedBinaryAPI
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPrivateField(cClass, "privateBinaryAPI")
      checkPublicMethod(cClass, "C$$inline$privateBinaryAPI", "()I")
      checkPublicMethod(cClass, "C$$inline$privateBinaryAPI_$eq", "(I)V")
      checkPublicMethod(cClass, "packagePrivateBinaryAPI", "()I")
      checkPublicMethod(cClass, "packagePrivateBinaryAPI_$eq", "(I)V")
      checkPublicMethod(cClass, "protectedBinaryAPI", "()I")
      checkPublicMethod(cClass, "protectedBinaryAPI_$eq", "(I)V")

      // Check that the @binaryAPI annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$privateBinaryAPI_$eq", "(I)V", false),
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateBinaryAPI_$eq", "(I)V", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedBinaryAPI_$eq", "(I)V", false),
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$privateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedBinaryAPI", "()I", false),
      ))
    }
  }

  @Test
  def binaryAPIGiven(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |class C:
        |  @binaryAPI private given privateBinaryAPI1: Int = 1
        |  @binaryAPI private[C] given packagePrivateBinaryAPI1: Int = 1
        |  @binaryAPI protected given protectedBinaryAPI1: Int = 1
        |  @binaryAPI private given privateBinaryAPI2(using Int): Int = 1
        |  @binaryAPI private[C] given packagePrivateBinaryAPI2(using Int): Int = 1
        |  @binaryAPI protected given protectedBinaryAPI2(using Int): Int = 1
        |  inline def inlined =
        |    packagePrivateBinaryAPI1 + protectedBinaryAPI1 + packagePrivateBinaryAPI2(using 1) + protectedBinaryAPI2(using 1)
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)
      checkPrivateMethod(cClass, "privateBinaryAPI1", "()I")
      checkPublicMethod(cClass, "C$$inline$privateBinaryAPI1", "()I")
      checkPublicMethod(cClass, "packagePrivateBinaryAPI1", "()I")
      checkPublicMethod(cClass, "protectedBinaryAPI1", "()I")

      checkPrivateMethod(cClass, "privateBinaryAPI2", "(I)I")
      checkPublicMethod(cClass, "C$$inline$privateBinaryAPI2", "(I)I")
      checkPublicMethod(cClass, "packagePrivateBinaryAPI2", "(I)I")
      checkPublicMethod(cClass, "protectedBinaryAPI2", "(I)I")

      // Check that the @binaryAPI annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateBinaryAPI1", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedBinaryAPI1", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateBinaryAPI2", "(I)I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedBinaryAPI2", "(I)I", false),
      ))
    }
  }

  @Test
  def binaryAPIClassParam(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |class C(
        |  @binaryAPI private val privateBinaryAPI: Int = 1,
        |  @binaryAPI private[C] val packagePrivateBinaryAPI: Int = 1,
        |  @binaryAPI protected val protectedBinaryAPI: Int = 1,
        |  @binaryAPI private var privateVarBinaryAPI: Int = 1
        |) {
        |  inline def inlined =
        |    privateVarBinaryAPI = 1
        |    privateBinaryAPI + packagePrivateBinaryAPI + protectedBinaryAPI + privateVarBinaryAPI
        |  def testInlined = inlined
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPrivateMethod(cClass, "privateBinaryAPI", "()I")
      checkPublicMethod(cClass, "C$$inline$privateBinaryAPI", "()I")
      checkPublicMethod(cClass, "packagePrivateBinaryAPI", "()I")
      checkPublicMethod(cClass, "protectedBinaryAPI", "()I")

      // Check that the @binaryAPI annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$privateVarBinaryAPI_$eq", "(I)V", false),
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$privateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$privateVarBinaryAPI", "()I", false),
      ))
    }
  }

  @Test
  def binaryAPIObject(): Unit = {
    val code =
      """package foo
        |import scala.annotation.binaryAPI
        |@binaryAPI private object PrivateBinaryAPI
        |@binaryAPI private[foo] object PackagePrivateBinaryAPI
        |@binaryAPI protected object ProtectedBinaryAPI
      """.stripMargin
    checkBCode(code) { dir =>
      val privateBinaryAPI = loadClassNode(dir.subdirectoryNamed("foo").lookupName("PrivateBinaryAPI$.class", directory = false).input, skipDebugInfo = false)
      checkPublicField(privateBinaryAPI, "MODULE$")

      val packagePrivateBinaryAPI = loadClassNode(dir.subdirectoryNamed("foo").lookupName("PackagePrivateBinaryAPI$.class", directory = false).input, skipDebugInfo = false)
      checkPublicField(packagePrivateBinaryAPI, "MODULE$")

      val protectedBinaryAPI = loadClassNode(dir.subdirectoryNamed("foo").lookupName("ProtectedBinaryAPI$.class", directory = false).input, skipDebugInfo = false)
      checkPublicField(protectedBinaryAPI, "MODULE$")
    }
  }

  @Test
  def binaryAPITraitDefs(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |trait C:
        |  @binaryAPI private val privateValBinaryAPI: Int = 1
        |  @binaryAPI private[C] val packagePrivateValBinaryAPI: Int = 1
        |  @binaryAPI protected val protectedValBinaryAPI: Int = 1
        |  @binaryAPI private lazy val privateLazyValBinaryAPI: Int = 1
        |  @binaryAPI private[C] lazy val packagePrivateLazyValBinaryAPI: Int = 1
        |  @binaryAPI protected lazy val protectedLazyValBinaryAPI: Int = 1
        |  @binaryAPI private var privateVarBinaryAPI: Int = 1
        |  @binaryAPI private[C] var packagePrivateVarBinaryAPI: Int = 1
        |  @binaryAPI protected var protectedVarBinaryAPI: Int = 1
        |  @binaryAPI private def privateDefBinaryAPI: Int = 1
        |  @binaryAPI private[C] def packagePrivateDefBinaryAPI: Int = 1
        |  @binaryAPI protected def protectedDefBinaryAPI: Int = 1
        |  inline def inlined =
        |    privateVarBinaryAPI = 1
        |    packagePrivateVarBinaryAPI = 1
        |    protectedVarBinaryAPI = 1
        |    privateValBinaryAPI +
        |    packagePrivateValBinaryAPI +
        |    protectedValBinaryAPI +
        |    privateLazyValBinaryAPI +
        |    packagePrivateLazyValBinaryAPI +
        |    protectedLazyValBinaryAPI +
        |    privateVarBinaryAPI +
        |    packagePrivateVarBinaryAPI +
        |    protectedVarBinaryAPI +
        |    privateDefBinaryAPI +
        |    packagePrivateDefBinaryAPI +
        |    protectedDefBinaryAPI
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cTrait = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPublicMethod(cTrait, "C$$privateValBinaryAPI", "()I")
      checkPublicMethod(cTrait, "packagePrivateValBinaryAPI", "()I")
      checkPublicMethod(cTrait, "protectedValBinaryAPI", "()I")
      checkPublicMethod(cTrait, "C$$privateLazyValBinaryAPI", "()I")
      checkPublicMethod(cTrait, "packagePrivateLazyValBinaryAPI", "()I")
      checkPublicMethod(cTrait, "protectedLazyValBinaryAPI", "()I")
      checkPublicMethod(cTrait, "C$$privateVarBinaryAPI", "()I")
      checkPublicMethod(cTrait, "packagePrivateVarBinaryAPI", "()I")
      checkPublicMethod(cTrait, "packagePrivateVarBinaryAPI_$eq", "(I)V")
      checkPublicMethod(cTrait, "protectedVarBinaryAPI", "()I")
      checkPublicMethod(cTrait, "protectedVarBinaryAPI_$eq", "(I)V")
      checkPublicMethod(cTrait, "packagePrivateDefBinaryAPI", "()I")
      checkPublicMethod(cTrait, "protectedDefBinaryAPI", "()I")

      // Check that the @binaryAPI annotated method is called
      val testInlined = getMethod(cTrait, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEINTERFACE, "C", "C$$inline$privateVarBinaryAPI_$eq", "(I)V", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateVarBinaryAPI_$eq", "(I)V", true),
        Invoke(INVOKEINTERFACE, "C", "protectedVarBinaryAPI_$eq", "(I)V", true),
        Invoke(INVOKEINTERFACE, "C", "C$$inline$privateValBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateValBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "protectedValBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "C$$inline$privateLazyValBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateLazyValBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "protectedLazyValBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "C$$inline$privateVarBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateVarBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "protectedVarBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "C$$inline$privateDefBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateDefBinaryAPI", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "protectedDefBinaryAPI", "()I", true)
      ))
    }
  }

  @Test
  def binaryAPIDefFinalPrivateAccessors(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |final class C(@binaryAPI paramBinaryAPI: Int):
        |  @binaryAPI private val valBinaryAPI: Int = 1
        |  @binaryAPI private def defBinaryAPI: Int = 1
        |  @binaryAPI private var varBinaryAPI: Int = 1
        |  inline def inlined =
        |    varBinaryAPI = 1
        |    paramBinaryAPI + valBinaryAPI + defBinaryAPI + varBinaryAPI
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPrivateField(cClass, "paramBinaryAPI")
      checkPublicMethod(cClass, "C$$inline$paramBinaryAPI", "()I")
      checkPrivateField(cClass, "valBinaryAPI")
      checkPublicMethod(cClass, "C$$inline$valBinaryAPI", "()I")
      checkPrivateMethod(cClass, "defBinaryAPI", "()I")
      checkPublicMethod(cClass, "C$$inline$defBinaryAPI", "()I")
      checkPrivateField(cClass, "varBinaryAPI")
      checkPublicMethod(cClass, "C$$inline$varBinaryAPI", "()I")
      checkPublicMethod(cClass, "C$$inline$varBinaryAPI_$eq", "(I)V")

      // Check that the @binaryAPI annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$varBinaryAPI_$eq", "(I)V", false),
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$paramBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$valBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$defBinaryAPI", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "C$$inline$varBinaryAPI", "()I", false),
      ))
    }
  }

  @Test
  def i13215(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |package foo:
        |  trait Bar:
        |    inline def baz = Baz
        |    def testInlined = baz
        |  @binaryAPI private[foo] object Baz
      """.stripMargin
    checkBCode(code) { dir =>
      val barClass = loadClassNode(dir.subdirectoryNamed("foo").lookupName("Bar.class", directory = false).input, skipDebugInfo = false)
      checkPublicMethod(barClass, "testInlined", "()Lfoo/Baz$;")
    }
  }

  @Test
  def i13215b(): Unit = {
    val code =
      """import scala.annotation.binaryAPI
        |package foo:
        |  trait Bar:
        |    inline def baz = Baz
        |    def testInlined = baz
        |  @binaryAPI private object Baz
      """.stripMargin
    checkBCode(code) { dir =>
      val barClass = loadClassNode(dir.subdirectoryNamed("foo").lookupName("Bar.class", directory = false).input, skipDebugInfo = false)
      checkPublicMethod(barClass, "testInlined", "()Lfoo/Baz$;")
    }
  }

  @Test
  def i15413(): Unit = {
    val code =
      """import scala.quoted.*
        |import scala.annotation.binaryAPI
        |class Macro:
        |  inline def foo = Macro.fooImpl
        |  def test = foo
        |object Macro:
        |  @binaryAPI private[Macro] def fooImpl = {}
      """.stripMargin
    checkBCode(code) { dir =>
      val macroClass = loadClassNode(dir.lookupName("Macro.class", directory = false).input, skipDebugInfo = false)
      val testMethod = getMethod(macroClass, "test")
      val testInstructions = instructionsFromMethod(testMethod).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInstructions, List(
        Invoke(INVOKEVIRTUAL, "Macro$", "fooImpl", "()V", false)))
    }
  }

  @Test
  def i15413b(): Unit = {
    val code =
      """package foo
        |import scala.annotation.binaryAPI
        |class C:
        |  inline def baz = D.bazImpl
        |  def test = baz
        |object D:
        |  @binaryAPI private[foo] def bazImpl = {}
      """.stripMargin
    checkBCode(code) { dir =>
      val barClass = loadClassNode(dir.subdirectoryNamed("foo").lookupName("C.class", directory = false).input, skipDebugInfo = false)
      val testMethod = getMethod(barClass, "test")
      val testInstructions = instructionsFromMethod(testMethod).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInstructions, List(
        Invoke(INVOKEVIRTUAL, "foo/D$", "bazImpl", "()V", false)))
    }
  }
}
