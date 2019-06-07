package dotty.tools.backend.sjs

import scala.annotation.tailrec

import dotty.tools.dotc._

import dotty.tools.dotc.core._
import Constants._
import Contexts._
import Decorators._
import Flags._
import Names._
import NameOps._
import Phases._
import Scopes._
import Symbols._
import StdNames._
import Types._

import dotty.tools.dotc.transform.MegaPhase._

/** Generates JUnit bootstrapper objects for Scala.js.
 *
 *  On the JVM, JUnit uses run-time reflection to list and invoke JUnit-related
 *  methods. They are identified by annotations such as `@Test`, `@Before`,
 *  etc. In Scala.js, there is no such reflection for methods and annotations,
 *  so a different strategy is used: this phase performs the necessary
 *  inspections at compile-time, and generates a so-called bootstrapper object
 *  where all those metadata have been reified.
 *
 *  With an example: given the following JUnit test class:
 *
 *  ```
 *  class MyTest {
 *    @Before def myBefore(): Unit = ...
 *    @Before def otherBefore(): Unit = ...
 *
 *    @Test def syncTest(): Unit = ...
 *    @Test def asyncTest(): Future[Try[Unit]] = ...
 *
 *    @Ignore @Test def ignoredTest(): Unit = ...
 *  }
 *
 *  object MyTest {
 *    @AfterClass def myAfterClass(): Unit = ...
 *  }
 *  ```
 *
 *  this phase generates the following bootstrapper module class:
 *
 *  ```
 *  object MyTest$scalajs$junit$bootstrapper extends Object with Bootstrapper {
 *    def beforeClass(): Unit = {
 *      // nothing, since there is no @BeforeClass method in object MyTest
 *    }
 *
 *    def afterClass(): Unit = {
 *      MyTest.myAfterClass()
 *    }
 *
 *    def before(instance: Object): Unit = {
 *      // typically 0 or 1, but also support 2 or more
 *      instance.asInstanceOf[MyTest].myBefore()
 *      instance.asInstanceOf[MyTest].otherBefore()
 *    }
 *
 *    def after(instance: Object): Unit = {
 *      // nothing, since there is no @After method in class MyTest
 *    }
 *
 *    def tests(): Array[TestMetadata] = Array(
 *      new TestMetadata("syncTest", false, new org.junit.Test()),
 *      new TestMetadata("asyncTest", false, new org.junit.Test()),
 *      new TestMetadata("ignoredTest", true, new org.junit.Test()),
 *    )
 *
 *    def invokeTest(instance: Object, name: String): Future[Unit] = {
 *      val castInstance: MyTest = instance.asInstanceOf[MyTest]
 *      if ("syncTest".equals(name))
 *        Future.successful(scala.util.Success(castInstance.syncTest()))
 *      else if ("asyncTest".equals(name))
 *        castInstance.asyncTest() // asyncTest() already returns a Future[Try[Unit]]
 *      else if ("ignoredTest".equals(name))
 *        Future.successful(scala.util.Success(castInstance.ignoredTest()))
 *      else
 *        throw new NoSuchMethodException(name)
 *    }
 *
 *    def newInstance(): Object = new MyTest()
 *  }
 *  ```
 *
 *  Note that the support for test methods returning `Future`s is specific to
 *  Scala.js, and not advertised as a public feature. It is necessary to test
 *  some things in Scala.js itself, but outside users should use a testing
 *  framework with official asynchronous support instead.
 *
 *  Because `Booststrapper` is annotated with `@EnableReflectiveInstantiation`,
 *  the run-time implementation of JUnit for Scala.js can load the boostrapper
 *  module using `scala.scalajs.reflect.Reflect`, and then use the methods of
 *  Bootstrapper, which are implemented in the bootstrapper object, to perform
 *  test discovery and invocation.
 *
 *  TODO At the moment, this phase does not handle `@Test` annotations with
 *  parameters, notably the expected exception class. This should be handled at
 *  some point in the future.
 */
class JUnitBootstrappers extends MiniPhase {
  import JUnitBootstrappers._
  import ast.tpd._
  import JSDefinitions.jsdefn

  def phaseName: String = "junitBootstrappers"

  override def isEnabled(implicit ctx: Context): Boolean =
    super.isEnabled && ctx.settings.scalajs.value

  // The actual transform -------------------------------

  override def transformPackageDef(tree: PackageDef)(implicit ctx: Context): Tree = {
    val junitdefn = jsdefn.junit

    @tailrec
    def hasTests(sym: ClassSymbol): Boolean = {
      sym.info.decls.exists(m => m.is(Method) && m.hasAnnotation(junitdefn.TestAnnotClass)) ||
      sym.superClass.exists && hasTests(sym.superClass.asClass)
    }

    def isTestClass(sym: Symbol): Boolean = {
      sym.isClass &&
      !sym.is(ModuleClass | Abstract | Trait) &&
      hasTests(sym.asClass)
    }

    val bootstrappers = tree.stats.collect {
      case clDef: TypeDef if isTestClass(clDef.symbol) =>
        genBootstrapper(clDef.symbol.asClass)
    }

    if (bootstrappers.isEmpty) tree
    else cpy.PackageDef(tree)(tree.pid, tree.stats ::: bootstrappers)
  }

  private def genBootstrapper(testClass: ClassSymbol)(implicit ctx: Context): TypeDef = {
    val junitdefn = jsdefn.junit

    /* The name of the boostrapper module. It is derived from the test class name by
     * appending a specific suffix string mandated "by spec". It will indeed also be
     * computed as such at run-time by the Scala.js JUnit Runtime support. Therefore,
     * it must *not* be a dotc semantic name.
     */
    val bootstrapperName = (testClass.name ++ "$scalajs$junit$bootstrapper").toTermName

    val owner = testClass.owner
    val moduleSym = ctx.newCompleteModuleSymbol(owner, bootstrapperName,
      Synthetic, Synthetic,
      List(defn.ObjectType, junitdefn.BootstrapperType), newScope,
      coord = testClass.span, assocFile = testClass.assocFile).entered
    val classSym = moduleSym.moduleClass.asClass

    val constr = genConstructor(classSym)

    val testMethods = annotatedMethods(testClass, junitdefn.TestAnnotClass)

    val defs = List(
      genCallOnModule(classSym, junitNme.beforeClass, testClass.companionModule, junitdefn.BeforeClassAnnotClass),
      genCallOnModule(classSym, junitNme.afterClass, testClass.companionModule, junitdefn.AfterClassAnnotClass),
      genCallOnParam(classSym, junitNme.before, testClass, junitdefn.BeforeAnnotClass),
      genCallOnParam(classSym, junitNme.after, testClass, junitdefn.AfterAnnotClass),
      genTests(classSym, testMethods),
      genInvokeTest(classSym, testClass, testMethods),
      genNewInstance(classSym, testClass)
    )

    sbt.APIUtils.registerDummyClass(classSym)

    ClassDef(classSym, constr, defs)
  }

  private def genConstructor(owner: ClassSymbol)(implicit ctx: Context): DefDef = {
    val sym = ctx.newDefaultConstructor(owner).entered
    DefDef(sym, {
      Block(
        Super(This(owner), nme.EMPTY.toTypeName, inConstrCall = true).select(defn.ObjectClass.primaryConstructor).appliedToNone :: Nil,
        unitLiteral
      )
    })
  }

  private def genCallOnModule(owner: ClassSymbol, name: TermName, module: Symbol, annot: Symbol)(implicit ctx: Context): DefDef = {
    val sym = ctx.newSymbol(owner, name, Synthetic | Method,
      MethodType(Nil, Nil, defn.UnitType)).entered

    DefDef(sym, {
      if (module.exists) {
        val calls = annotatedMethods(module.moduleClass.asClass, annot)
          .map(m => Apply(ref(module).select(m), Nil))
        Block(calls, unitLiteral)
      } else {
        unitLiteral
      }
    })
  }

  private def genCallOnParam(owner: ClassSymbol, name: TermName, testClass: ClassSymbol, annot: Symbol)(implicit ctx: Context): DefDef = {
    val sym = ctx.newSymbol(owner, name, Synthetic | Method,
      MethodType(junitNme.instance :: Nil, defn.ObjectType :: Nil, defn.UnitType)).entered

    DefDef(sym, { (paramRefss: List[List[Tree]]) =>
      val List(List(instanceParamRef)) = paramRefss
      val calls = annotatedMethods(testClass, annot)
        .map(m => Apply(instanceParamRef.cast(testClass.typeRef).select(m), Nil))
      Block(calls, unitLiteral)
    })
  }

  private def genTests(owner: ClassSymbol, tests: List[Symbol])(implicit ctx: Context): DefDef = {
    val junitdefn = jsdefn.junit

    val sym = ctx.newSymbol(owner, junitNme.tests, Synthetic | Method,
      MethodType(Nil, defn.ArrayOf(junitdefn.TestMetadataType))).entered

    DefDef(sym, {
      val metadata = for (test <- tests) yield {
        val name = Literal(Constant(test.name.toString))
        val ignored = Literal(Constant(test.hasAnnotation(junitdefn.IgnoreAnnotClass)))
        // TODO Handle @Test annotations with arguments
        // val reifiedAnnot = New(mydefn.TestAnnotType, test.getAnnotation(mydefn.TestAnnotClass).get.arguments)
        val testAnnot = test.getAnnotation(junitdefn.TestAnnotClass).get
        if (testAnnot.arguments.nonEmpty)
          ctx.error("@Test annotations with arguments are not yet supported in Scala.js for dotty", testAnnot.tree.sourcePos)
        val noArgConstr = junitdefn.TestAnnotType.member(nme.CONSTRUCTOR).suchThat(_.info.paramInfoss.head.isEmpty).symbol.asTerm
        val reifiedAnnot = New(junitdefn.TestAnnotType, noArgConstr, Nil)
        New(junitdefn.TestMetadataType, List(name, ignored, reifiedAnnot))
      }
      JavaSeqLiteral(metadata, TypeTree(junitdefn.TestMetadataType))
    })
  }

  private def genInvokeTest(owner: ClassSymbol, testClass: ClassSymbol, tests: List[Symbol])(implicit ctx: Context): DefDef = {
    val junitdefn = jsdefn.junit

    val sym = ctx.newSymbol(owner, junitNme.invokeTest, Synthetic | Method,
      MethodType(List(junitNme.instance, junitNme.name), List(defn.ObjectType, defn.StringType), junitdefn.FutureType)).entered

    DefDef(sym, { (paramRefss: List[List[Tree]]) =>
      val List(List(instanceParamRef, nameParamRef)) = paramRefss
      val castInstanceSym = ctx.newSymbol(sym, junitNme.castInstance, Synthetic, testClass.typeRef, coord = owner.span)
      Block(
        ValDef(castInstanceSym, instanceParamRef.cast(testClass.typeRef)) :: Nil,
        tests.foldRight[Tree] {
          val tp = junitdefn.NoSuchMethodExceptionType
          val constr = tp.member(nme.CONSTRUCTOR).suchThat { c =>
            c.info.paramInfoss.head.size == 1 &&
            c.info.paramInfoss.head.head.isRef(defn.StringClass)
          }.symbol.asTerm
          Throw(New(tp, constr, nameParamRef :: Nil))
        } { (test, next) =>
          If(Literal(Constant(test.name.toString)).select(defn.Any_equals).appliedTo(nameParamRef),
            genTestInvocation(testClass, test, ref(castInstanceSym)),
            next)
        }
      )
    })
  }

  private def genTestInvocation(testClass: ClassSymbol, testMethod: Symbol, instance: Tree)(implicit ctx: Context): Tree = {
    val junitdefn = jsdefn.junit

    val resultType = testMethod.info.resultType
    if (resultType.isRef(defn.UnitClass)) {
      val newSuccess = ref(junitdefn.SuccessModule_apply).appliedTo(ref(defn.BoxedUnit_UNIT))
      Block(
        instance.select(testMethod).appliedToNone :: Nil,
        ref(junitdefn.FutureModule_successful).appliedTo(newSuccess)
      )
    } else if (resultType.isRef(junitdefn.FutureClass)) {
      instance.select(testMethod).appliedToNone
    } else {
      // We lie in the error message to not expose that we support async testing.
      ctx.error("JUnit test must have Unit return type", testMethod.sourcePos)
      EmptyTree
    }
  }

  private def genNewInstance(owner: ClassSymbol, testClass: ClassSymbol)(implicit ctx: Context): DefDef = {
    val sym = ctx.newSymbol(owner, junitNme.newInstance, Synthetic | Method,
      MethodType(Nil, defn.ObjectType)).entered

    DefDef(sym, New(testClass.typeRef, Nil))
  }

  private def castParam(param: Symbol, clazz: Symbol)(implicit ctx: Context): Tree =
    ref(param).cast(clazz.typeRef)

  private def annotatedMethods(owner: ClassSymbol, annot: Symbol)(implicit ctx: Context): List[Symbol] =
    owner.info.decls.filter(m => m.is(Method) && m.hasAnnotation(annot))
}

object JUnitBootstrappers {

  private object junitNme {
    val beforeClass: TermName = termName("beforeClass")
    val afterClass: TermName = termName("afterClass")
    val before: TermName = termName("before")
    val after: TermName = termName("after")
    val tests: TermName = termName("tests")
    val invokeTest: TermName = termName("invokeTest")
    val newInstance: TermName = termName("newInstance")

    val instance: TermName = termName("instance")
    val name: TermName = termName("name")
    val castInstance: TermName = termName("castInstance")
  }

}
