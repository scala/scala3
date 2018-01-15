package dotty
package tools
package dotc

import org.junit.{AfterClass, Test}
import vulpix._

import scala.concurrent.duration._

class FromTastyTests extends ParallelTesting {
  import TestConfiguration._
  import FromTastyTests._

  // Test suite configuration --------------------------------------------------

  def maxDuration = 30.seconds
  def numberOfSlaves = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter


  @Test def posTestFromTasty: Unit = {
    // Can be reproduced with
    // > sbt
    // > dotc -Ythrough-tasty -Ycheck:all <source>

    implicit val testGroup: TestGroup = TestGroup("posTestFromTasty")
    val (step1, step2) = compileTastyInDir("../tests/pos", defaultOptions,
      blacklist = Set(
        /* assertion failed: owner discrepancy for type CC, expected: class <refinement>, found: class <refinement>
            at scala.Predef$.assert(Predef.scala:219)
            at dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader.symbolAtCurrent(TreeUnpickler.scala:195)
         */
        "NoCyclicReference.scala",
        "i1795.scala",

        /* Exception in thread "main" dotty.tools.dotc.core.Types$MergeError: cannot merge (implicit x$0: Test.C): Test.C#M with (x: Test.C): x.M as members of type Test.IDF(Test.ifun)
            at dotty.tools.dotc.core.Denotations$Denotation.mergeSingleDenot$1(Denotations.scala:527)
         */
        "depfuntype.scala",

        /* Exception in thread "main" java.lang.AssertionError: assertion failed: found:    collection.generic.GenericCompanion[collection.immutable.Seq](Test.a)
            required: scala.collection.generic.GenericCompanion[
              [X0] => scala.collection.immutable.Seq[X0] | scala.collection.mutable.Seq'[X0]
            ]

            where:    Seq  is a trait in package immutable
                      Seq' is a trait in package mutable

            tree = Test.a
              at scala.Predef$.assert(Predef.scala:219)
              at dotty.tools.dotc.transform.TreeChecker$Checker.adapt(TreeChecker.scala:448)
         */
        "hklub0.scala",
        "i1365.scala",

        /* assertion failed: position not set for Ident(u) # 1086
	          at scala.Predef$.assert(Predef.scala:219)
	          at dotty.tools.dotc.typer.Typer$.assertPositioned(Typer.scala:63)
         */
        "i0306.scala",

        /* java.lang.NullPointerException
            at dotty.tools.dotc.core.Hashable.finishHash(Hashable.scala:65)
            at dotty.tools.dotc.core.Hashable.finishHash$(Hashable.scala:64)
            at dotty.tools.dotc.core.Types$Type.finishHash(Types.scala:87)
            at dotty.tools.dotc.core.Hashable.doHash(Hashable.scala:89)
            at dotty.tools.dotc.core.Hashable.doHash$(Hashable.scala:88)
            at dotty.tools.dotc.core.Types$Type.doHash(Types.scala:87)
            at dotty.tools.dotc.core.Types$HKLambda.computeHash(Types.scala:2639)
         */
        "i2888.scala",
        "i974.scala",
        "t3800.scala",

        /* assertion failed: found:    b
            required: b'

            where:    b  is a type in method foo with bounds
                      b' is a type in method foo with bounds

            tree = 3.asInstanceOf[b]
              at scala.Predef$.assert(Predef.scala:219)
              at dotty.tools.dotc.transform.TreeChecker$Checker.adapt(TreeChecker.scala:448)
         */
        "i2944.scala",
        "t8023.scala",

        /* assertion failed: position not set for Ident(bar) # 502
            at scala.Predef$.assert(Predef.scala:219)
            at dotty.tools.dotc.typer.Typer$.assertPositioned(Typer.scala:63)
         */
        "i3000.scala",
        "t1203a.scala",
        "t2260.scala",
        "t4579.scala",
        "tcpoly_ticket2096.scala",
        "t247.scala",
        "i2345.scala",

        /* cyclic reference involving type M
            at dotty.tools.dotc.core.Types$CyclicReference$.apply(Types.scala:4416)
            at dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:244)
            at dotty.tools.dotc.core.SymDenotations$SymDenotation.completeInfo$1(SymDenotations.scala:209)
            at dotty.tools.dotc.core.SymDenotations$SymDenotation.info(SymDenotations.scala:211)
            at dotty.tools.dotc.core.Types$NamedType.info(Types.scala:1642)
            at dotty.tools.dotc.core.Types$TypeRef.underlying(Types.scala:2072)
            at dotty.tools.dotc.core.Types$Type.phantomLatticeType(Types.scala:246)
         */
        "i536.scala",

        /* assertion failed:  object a tree does not define methods: method $anonfun
            expected: Set(method <init>, method $anonfun)%, %
            defined:
              at scala.Predef$.assert(Predef.scala:219)
              at dotty.tools.dotc.transform.TreeChecker$Checker.typedClassDef(TreeChecker.scala:385)
         */
        "i3067.scala",

        // Infinite compilation
        "t3612.scala",
      )
    )
    step1.checkCompile() // Compile all files to generate the class files with tasty
    step2.checkCompile() // Compile from tasty
    (step1 + step2).delete()
  }

  @Test def runTestFromTasty: Unit = {
    // Can be reproduced with
    // > sbt
    // > dotc -Ythrough-tasty -Ycheck:all <source>
    // > dotr Test

    implicit val testGroup: TestGroup = TestGroup("runTestFromTasty")
    val (step1, step2) = compileTastyInDir("../tests/run", defaultOptions,
       blacklist = Set(
         "t3613.scala",

         /* position not set for DefDef(min,List(),List(List(ValDef(cmp,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class math)),class Ordering)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class Object)],Apply(Select(This(Ident(Tokenizer)),min),List(Ident(cmp)))) # 44104
            at scala.Predef$.assert(Predef.scala:219)
            at dotty.tools.dotc.typer.Typer$.assertPositioned(Typer.scala:63)
          */
         "Course-2002-13.scala",
         "bridges.scala",
         "i2337.scala",
         "i2337b.scala",
         "enum-approx.scala",
         "inlineForeach.scala",
         "scala2trait-lazyval.scala",
         "t3452f.scala",

         /* assertion failed: found:    (f: Test.Fun[A, B]) => (g: Test.Fun[B, C]) => A => implicit f.Eff => implicit
              g.Eff
             => C
            required: {z1 => (f: Test.Fun[A, B]) => (g: Test.Fun[B, C]) => A => implicit z1.f.Eff =>
              implicit
            g.Eff => C}


            tree = closure($anonfun)
              at scala.Predef$.assert(Predef.scala:219)
              at dotty.tools.dotc.transform.TreeChecker$Checker.adapt(TreeChecker.scala:448)
          */
         "eff-dependent.scala",

         /* dotty.tools.dotc.core.tasty.TreeUnpickler$TreeWithoutOwner
	           at dotty.tools.dotc.core.tasty.TreeUnpickler$OwnerTree.search$1(TreeUnpickler.scala:1156)
          */
         "patmat-bind-typed.scala",
         "t8395.scala",

         /* Issue unpickling universes */
         "phantom-decls-1.scala",
         "phantom-decls-3.scala",
         "phantom-decls-5.scala",
         "phantom-hk-1.scala",
         "phantom-hk-2.scala",
         "phantom-in-value-class.scala",
         "phantom-methods-3.scala",
         "phantom-methods-4.scala",
         "phantom-poly-1.scala",
         "phantom-poly-2.scala",
         "phantom-poly-3.scala",
         "phantom-poly-4.scala",
       )
    )
    step1.checkCompile() // Compile all files to generate the class files with tasty
    step2.checkRuns() // Compile from tasty and run the result
    (step1 + step2).delete()
  }

  private implicit class tastyCompilationTuples(tup: (CompilationTest, CompilationTest)) {
    def +(that: (CompilationTest, CompilationTest)): (CompilationTest, CompilationTest) =
      (tup._1 + that._1, tup._2 + that._2)
  }
}

object FromTastyTests {
  implicit val summaryReport: SummaryReporting = new SummaryReport
  @AfterClass def cleanup(): Unit = summaryReport.echoSummary()
}
