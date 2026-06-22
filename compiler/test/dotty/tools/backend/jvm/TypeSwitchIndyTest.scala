package dotty.tools.backend.jvm

import dotty.DottyBytecodeTest

import scala.language.unsafeNulls
import org.junit.Test
import org.junit.Assert.*

import scala.tools.asm.Opcodes.*

class TypeSwitchIndyTest extends DottyBytecodeTest {
  import dotty.AsmConverters.*

  override def initCtx = {
    val ctx0 = super.initCtx
    ctx0.setSetting(ctx0.settings.YemitTypeSwitchIndy, true)
    ctx0.setSetting(ctx0.settings.XuncheckedJavaOutputVersion, "21")
    ctx0
  }

  @Test def typeSwitchObject(): Unit = {
    val source =
      """class C {
        |  def run(o: Object): Int = o match
        |    case s: String           => s.length
        |    case i: java.lang.Integer => i.intValue
        |    case _: java.lang.Long   => 3
        |    case _: java.lang.Float  => 4
        |    case _                   => 0
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsNode = loadClassNode(dir.lookupName("C.class", directory = false).input)
      val instrs  = instructionsFromMethod(getMethod(clsNode, "run"))

      val indys = instrs.collect { case i: InvokeDynamic => i }

      assertEquals("expected exactly one invokedynamic", 1, indys.size)

      val indy = indys.head
      assertEquals("typeSwitch", indy.name)
      assertEquals("(Ljava/lang/Object;I)I", indy.desc)
      assertEquals("java/lang/runtime/SwitchBootstraps", indy.bsm.owner)
      assertEquals("typeSwitch", indy.bsm.name)
    }
  }

  @Test def enumSwitchJavaEnum(): Unit = {
    val source =
      """import java.util.concurrent.TimeUnit
        |class C {
        |  def run(u: TimeUnit): Int = u match
        |    case TimeUnit.SECONDS => 1
        |    case TimeUnit.MINUTES => 2
        |    case TimeUnit.HOURS   => 3
        |    case _                => 0
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsNode = loadClassNode(dir.lookupName("C.class", directory = false).input)
      val instrs  = instructionsFromMethod(getMethod(clsNode, "run"))

      val indys = instrs.collect { case i: InvokeDynamic => i }

      assertEquals("expected exactly one invokedynamic", 1, indys.size)

      val indy = indys.head
      assertEquals("enumSwitch", indy.name)
      assertEquals("(Ljava/util/concurrent/TimeUnit;I)I", indy.desc)
      assertEquals("java/lang/runtime/SwitchBootstraps", indy.bsm.owner)
      assertEquals("enumSwitch", indy.bsm.name)
    }
  }
}
