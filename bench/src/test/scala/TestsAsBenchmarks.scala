package dotty.tools.benchmarks

import java.lang.annotation.Annotation
import java.lang.reflect.Method

import org.junit.runner.Request
import org.junit.runner.notification.RunNotifier
import org.scalameter.PerformanceTest.OnlineRegressionReport
import org.scalameter.api._
import org.scalameter.reporting.RegressionReporter.Tester

import scala.collection.mutable.ListBuffer


abstract class TestsToBenchmarkConverter
(targetClass: Class[_],
 filterAnnot: Class[_ <: java.lang.annotation.Annotation] = classOf[org.junit.Test].asInstanceOf[Class[_ <: java.lang.annotation.Annotation]])
  extends OnlineRegressionReport {

  // accept all the results, do not fail
  override def tester: Tester = new Tester.Accepter

  override def executor: Executor = LocalExecutor(warmer, aggregator, measurer)
  val testNames = getMethodsAnnotatedWith(targetClass, filterAnnot).map(_.getName).sorted


  val tests = testNames.map{name =>
    val runner = Request.method(targetClass, name).getRunner
    (name, Gen.single("test")(name).map(Request.method(targetClass, _).getRunner))}.toMap
  //Gen.enumeration("test")(testNames:_*)

  performance of targetClass.getSimpleName config (Context(reports.resultDir -> "./tmp")) in {
    for (test <- testNames)
      measure.method(test) in {
        using(tests(test)) curve test in {
          r =>
            val dummy = new RunNotifier()
            r.run(dummy)
        }
      }
  }

  def getMethodsAnnotatedWith(clazz: Class[_], annotation: Class[_ <: java.lang.annotation.Annotation]): List[Method] = {
    val methods = ListBuffer[Method]()
    var klass: Class[_] = clazz
    while (klass ne classOf[AnyRef]) {
      val allMethods = klass.getDeclaredMethods
      import scala.collection.JavaConversions._
      for (method <- allMethods) {
        if (annotation == null || method.isAnnotationPresent(annotation)) {
          val annotInstance: Annotation = method.getAnnotation(annotation)
          methods.add(method)
        }
      }
      klass = klass.getSuperclass
    }
    methods.toList
  }
}

object dotcTests extends TestsToBenchmarkConverter(classOf[dotc.tests])
