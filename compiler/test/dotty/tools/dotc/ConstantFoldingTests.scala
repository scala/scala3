package dotty.tools.dotc

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test
import dotty.tools.backend.jvm._
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.dotc.core.Contexts.FreshContext
import scala.tools.asm.tree.MethodNode

import scala.jdk.CollectionConverters._

class ConstantFoldingTests extends DottyBytecodeTest {

  val conditionSrc = """class Test {
  def someCondition: Boolean = ???
  final val t = true
  final val f = false

  def whenTrue  =
    println("hi")
  def whenFalse =
    ()
  def whenCond  =
    if (someCondition)
      println("hi")

  def reduceToTrue1 =
    if (t)
      println("hi")
  def reduceToTrue2 =
    if (t || someCondition)
      println("hi")
  def reduceToTrue3 =
    if (f || t || someCondition)
      println("hi")

  def reduceToFalse1 =
    if (f)
      println("hi")
  def reduceToFalse2 =
    if (f && someCondition)
      println("hi")
  def reduceToFalse3 =
    if (t && f && someCondition)
      println("hi")

  def reduceToCond1 =
    if (t && someCondition)
      println("hi")
  def reduceToCond2 =
    if (f || someCondition)
      println("hi")
  def reduceToCond3 =
    if ((t && f) || someCondition)
      println("hi")
}
"""

  @Test def constantFoldConditions: Unit = {
    import ASMConverters._

    checkBCode(conditionSrc) { dir =>
      val clsIn   = dir.lookupName(s"Test.class", directory = false).input
      val methods = loadClassNode(clsIn).methods.asScala

      val whenTrue = methods.find(_.name == "whenTrue").get
      val whenFalse = methods.find(_.name == "whenFalse").get
      val whenCond = methods.find(_.name == "whenCond").get

      val reduceToTrue = methods.filter(_.name.startsWith("reduceToTrue"))
      val reduceToFalse = methods.filter(_.name.startsWith("reduceToFalse"))
      val reduceToCond = methods.filter(_.name.startsWith("reduceToCond"))

      def compare(expected: MethodNode, actual: MethodNode) = {
        val expectedInstrs = instructionsFromMethod(expected)
        val actualInstrs = instructionsFromMethod(actual)
        val diff = diffInstructions(expectedInstrs, actualInstrs)
        assert(expectedInstrs == actualInstrs,
          s"Different bytecode between ${expected.name} and ${actual.name}\n$diff")
      }
      reduceToTrue.foreach(compare(whenTrue, _))
      reduceToFalse.foreach(compare(whenFalse, _))
      reduceToCond.foreach(compare(whenCond, _))
    }
  }
}
