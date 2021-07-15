package xsbt

import org.junit.Assert.assertEquals
import org.junit.Test

class CompileProgressReportTest:

  @Test
  def testStartUnitReports(): Unit =
    val source1 =
      """class A {
        |
        |}
        |""".stripMargin

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val progress = compilerForTesting.compileSrcs(source1)._3

    val expectedProgress = Seq(
      ("Test-0-0.scala", "typer (parsing)"),
      ("Test-0-0.scala", "typer (typechecking)"),
      ("Test-0-0.scala", "typer (checking java)"),
      ("Test-0-0.scala", "inlinedPositions"),
      ("Test-0-0.scala", "sbt-deps"),
      ("Test-0-0.scala", "posttyper"),
      ("Test-0-0.scala", "sbt-api"),
      ("Test-0-0.scala", "pickler"),
      ("Test-0-0.scala", "inlining"),
      ("Test-0-0.scala", "postInlining"),
      ("Test-0-0.scala", "staging"),
      ("Test-0-0.scala", "pickleQuotes"),
      ("Test-0-0.scala", "MegaPhase{firstTransform, checkReentrant, elimPackagePrefixes, cookComments, checkStatic, betaReduce, inlineVals, expandSAMs}"),
      ("Test-0-0.scala", "MegaPhase{elimRepeated, protectedAccessors, extmethods, uncacheGivenAliases, byNameClosures, hoistSuperArgs, specializeApplyMethods, refchecks}"),
      ("Test-0-0.scala", "MegaPhase{elimOpaque, tryCatchPatterns, patternMatcher, explicitOuter, explicitSelf, elimByName, stringInterpolatorOpt}"),
      ("Test-0-0.scala", "MegaPhase{pruneErasedDefs, uninitializedDefs, inlinePatterns, vcInlineMethods, seqLiterals, intercepted, getters, specializeFunctions, liftTry, collectNullableFields, elimOuterSelect, resolveSuper, functionXXLForwarders, paramForwarding, genericTuples, letOverApply, arrayConstructors}"),
      ("Test-0-0.scala", "erasure"),
      ("Test-0-0.scala", "MegaPhase{elimErasedValueType, pureStats, vcElideAllocations, arrayApply, elimPolyFunction, tailrec, completeJavaEnums, mixin, lazyVals, memoize, nonLocalReturns, capturedVars}"),
      ("Test-0-0.scala", "constructors"),
      ("Test-0-0.scala", "MegaPhase{lambdaLift, elimStaticThis, countOuterAccesses}"),
      ("Test-0-0.scala", "MegaPhase{dropOuterAccessors, checkNoSuperThis, flatten, renameLifted, transformWildcards, moveStatic, expandPrivate, restoreScopes, selectStatic, collectSuperCalls, repeatableAnnotations}"),
      ("Test-0-0.scala", "genBCode")
    )

    val actualProgress = progress.startUnitCalls.map { case (phase, filePath0) =>
      val filePath = filePath0.replace("\\", "/") // for Windows
      val fileNameShort = filePath.substring(filePath.lastIndexOf("/") + 1, filePath.length)
      (fileNameShort, phase)
    }

    // .mkString("\n") for better diff view
    assertEquals(expectedProgress.mkString("\n"), actualProgress.mkString("\n"))
