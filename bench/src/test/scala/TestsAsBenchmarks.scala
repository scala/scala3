package dotty.tools.benchmarks

import java.lang.annotation.Annotation
import java.lang.reflect.Method

import org.junit.runner.Request
import org.junit.runner.notification.RunNotifier
import org.scalameter.Key.reports._
import org.scalameter.PerformanceTest.OnlineRegressionReport
import org.scalameter.api._
import org.scalameter.{Context, History, persistence, currentContext}
import org.scalameter.reporting.RegressionReporter.Tester

import scala.collection.mutable.ListBuffer

// decorator of persitor to expose info for debugging
class DecoratorPersistor(p: Persistor) extends SerializationPersistor {

  override def load(context: Context): History = {
    val resultdir = currentContext(resultDir)
    val scope = context.scope
    val curve = context.curve
    val fileName = s"$resultdir$sep$scope.$curve.dat"

    println(s"load file $fileName")

    p.load(context)
  }

  override def save(context: Context, h: History) = {
    val resultdir = currentContext(resultDir)
    val scope = context.scope
    val curve = context.curve
    val fileName = s"$resultdir$sep$scope.$curve.dat"

    println(s"save file $fileName")

    p.save(context, h)
  }
}

abstract class TestsToBenchmarkConverter
(targetClass: Class[_],
 filterAnnot: Class[_ <: java.lang.annotation.Annotation] = classOf[org.junit.Test].asInstanceOf[Class[_ <: java.lang.annotation.Annotation]])
  extends OnlineRegressionReport {

  // NOTE: use `val persistor = ...` would cause persistor ignore command line options for `resultDir`
  override def persistor = new DecoratorPersistor(super.persistor)

  // accept all the results, do not fail
  override def tester: Tester = new Tester.Accepter

  // store all results
  override def historian: RegressionReporter.Historian = RegressionReporter.Historian.Complete()

  override def executor: Executor = LocalExecutor(warmer, aggregator, measurer)
  val testNames = getMethodsAnnotatedWith(targetClass, filterAnnot).map(_.getName).sorted


  val tests = testNames.map{name =>
    val runner = Request.method(targetClass, name).getRunner
    (name, Gen.single("test")(name).map(Request.method(targetClass, _).getRunner))}.toMap
  //Gen.enumeration("test")(testNames:_*)

  def setup =
    performance of targetClass.getSimpleName in {
      for (test <- testNames)
        measure.method(test) in {
          using(tests(test)) curve test in {
            r =>
              val dummy = new RunNotifier()
              r.run(dummy)
          }
        }
    }

  /** workaround to fix problem in ScalaMeter
    *
    * NOTE: Otherwise, command line options would be ignored by HTMLReporter, as
    *       the HTMLReporter uses the context of tree node, which is created via
    *       ScalaMeter DSL before command line option `-CresultDir` takes effect
    *       in `PerformanceTest.main`.
    *
    *       Following code ensures that the test tree is set up after the `-CresultDir`
    *       option takes effect.
    **/
  override def executeTests(): Boolean = {
    setup
    super.executeTests()
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
