package dotty.tools.pc.base

import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls

import org.junit.runners.BlockJUnit4ClassRunner
import org.junit.runners.model.FrameworkMethod
import org.junit.runners.model.Statement

class ReusableClassRunner(testClass: Class[BasePCSuite])
    extends BlockJUnit4ClassRunner(testClass):
  private val instance: BasePCSuite =
    testClass.getDeclaredConstructor().newInstance()

  override def createTest(): AnyRef = instance

  override def withAfters(
      method: FrameworkMethod,
      target: Object,
      statement: Statement
  ): Statement =
    val newStatement = super.withAfters(method, target, statement)
    new Statement():
      override def evaluate(): Unit =
        try
          newStatement.evaluate()
        finally
          if isLastTestCase(method) then instance.clean()

  private def isLastTestCase(method: FrameworkMethod): Boolean =
    val testMethods =
      getTestClass().getAnnotatedMethods(classOf[org.junit.Test])
    testMethods.asScala.last == method
