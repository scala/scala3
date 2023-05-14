package dotty.tools.dotc.ast

import dotty.tools.DottyTest
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.util.Property

import org.junit.Test
import org.junit.Assert.{assertEquals, assertTrue, fail}

class AttachmentsTests extends DottyTest {

  private val TestKey = new Property.Key[String]
  private val StickyTestKey = new Property.StickyKey[String]
  private val StickyTestKey2 = new Property.StickyKey[String]

  @Test
  def attachmentsAreNotCopiedOver: Unit = {
    checkCompile("typer", "class A") {
      case (PackageDef(_, (clazz: tpd.TypeDef) :: Nil), context) =>
        assertTrue("Attachment shouldn't be present", clazz.getAttachment(TestKey).isEmpty)

        val msg = "hello"
        clazz.putAttachment(TestKey, msg)
        assertEquals(Some(msg), clazz.getAttachment(TestKey))

        val copy = tpd.cpy.TypeDef(clazz)(rhs = tpd.EmptyTree)
        assertTrue("A copy should have been returned", clazz ne copy)
        assertTrue("Attachment shouldn't be present", copy.getAttachment(TestKey).isEmpty)

      case _ =>
        fail
    }
  }

  @Test
  def stickyAttachmentsAreCopiedOver: Unit = {
    checkCompile("typer", "class A") {
      case (PackageDef(_, (clazz: tpd.TypeDef) :: Nil), context) =>
        assertTrue("Attachment shouldn't be present", clazz.getAttachment(StickyTestKey).isEmpty)
        assertTrue("Attachment shouldn't be present", clazz.getAttachment(StickyTestKey2).isEmpty)
        assertTrue("Attachment shouldn't be present", clazz.getAttachment(TestKey).isEmpty)

        val msg = "hello"
        clazz.putAttachment(StickyTestKey, msg)
        clazz.putAttachment(TestKey, msg)
        clazz.putAttachment(StickyTestKey2, msg)
        assertEquals(Some(msg), clazz.getAttachment(StickyTestKey))
        assertEquals(Some(msg), clazz.getAttachment(TestKey))
        assertEquals(Some(msg), clazz.getAttachment(StickyTestKey))

        val copy = tpd.cpy.TypeDef(clazz)(rhs = tpd.EmptyTree)
        assertTrue("A copy should have been returned", clazz ne copy)
        assertTrue("Attachment should be present", copy.hasAttachment(StickyTestKey))
        assertTrue("Attachment shouldn't be present", !copy.hasAttachment(TestKey))
        assertTrue("Attachment should be present", copy.hasAttachment(StickyTestKey2))

      case _ =>
        fail
    }
  }

}
