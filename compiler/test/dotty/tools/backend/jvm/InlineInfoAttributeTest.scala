package dotty.tools.backend.jvm

import dotty.DottyBytecodeTest
import dotty.tools.backend.jvm.opt.{InlineInfoAttribute, InlineInfoAttributePrototype}
import org.junit.Assert.*
import org.junit.Test
import scala.tools.asm.{Attribute, ClassReader}
import scala.tools.asm.tree.ClassNode

import scala.jdk.CollectionConverters.*

class InlineInfoAttributeTest extends DottyBytecodeTest {
  override def initCtx = {
    val ctx = super.initCtx
    ctx.setSetting(ctx.settings.opt, true)
    ctx.setSetting(ctx.settings.optInline, List("**", "!java.**"))
  }

  // Each class mentions the other in a generic signature, so whichever is emitted first creates
  // the ClassBType of the other one early. Its InlineInfo must still list the lifted lambda body.
  @Test def liftedMethodsInInlineInfo(): Unit = {
    val source =
      """class A { def b: Option[B] = None; def f(xs: List[Int]) = xs.map(x => x + 1) }
        |class B { def a: Option[A] = None; def f(xs: List[Int]) = xs.map(x => x + 1) }
      """.stripMargin

    checkBCode(source) { dir =>
      for cls <- List("A", "B") do
        val cn = new ClassNode()
        new ClassReader(dir.lookupName(s"$cls.class", directory = false).nn.input).accept(cn, Array[Attribute](InlineInfoAttributePrototype), ClassReader.SKIP_FRAMES)
        val inlineInfo = cn.attrs.asScala.collectFirst { case a: InlineInfoAttribute => a.inlineInfo }.get
        val lifted = cn.methods.asScala.map(m => (m.name, m.desc)).filter(_._1.contains("$anonfun$")).toSet
        assertTrue(s"no lifted lambda body in $cls", lifted.nonEmpty)
        assertEquals(cls, lifted, inlineInfo.methodInfos.keySet.intersect(lifted))
    }
  }
}
