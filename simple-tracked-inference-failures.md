sbt:scala3> testCompilation
[info] Test run dotty.tools.dotc.CompilationTests started
[info] Test dotty.tools.dotc.CompilationTests.parallelBackend started
[                                        ] completed (0/7, 0 failed, 2s)[warn] Metals requires the semanticdb compiler plugin
[warn] consider setting 'Global / semanticdbEnabled := true' in your global sbt settings ($HOME/.sbt/1.0)
[=======================================>] completed (7/7, 0 failed, 18s)
[=======================================>] completed (2/2, 0 failed, 0s)
[=======================================>] completed (2/2, 0 failed, 2s)
[info] Test dotty.tools.dotc.CompilationTests.negAll started
[===>                                    ] completed (233/1928, 0 failed, 8s)
No errors found when compiling neg test tests/neg/experimental-nested-imports-2.scala
[====>                                   ] completed (257/1928, 1 failed, 8s)
No errors found when compiling neg test tests/neg/experimentalDefaultParams.scala
[=====>                                  ] completed (300/1928, 2 failed, 10s)
No errors found when compiling neg test tests/neg/expeimental-flag.scala
[========>                               ] completed (449/1928, 3 failed, 14s)
No errors found when compiling neg test tests/neg/experimental.scala
[=========>                              ] completed (489/1928, 4 failed, 14s)
No errors found when compiling neg test tests/neg/experimentalTypes2.scala
[==========>                             ] completed (537/1928, 5 failed, 16s)
No errors found when compiling neg test tests/neg/experimental-message.scala
[============>                           ] completed (649/1928, 6 failed, 18s)
No errors found when compiling neg test tests/neg/experimental-nested-imports-3.scala
[============>                           ] completed (671/1928, 7 failed, 18s)
No errors found when compiling neg test tests/neg/i13848.scala
[=============>                          ] completed (687/1928, 8 failed, 19s)
No errors found when compiling neg test tests/neg/experimentalUnapply.scala
[=============>                          ] completed (696/1928, 9 failed, 19s)
No errors found when compiling neg test tests/neg/experimentalType.scala
[===============>                        ] completed (779/1928, 10 failed, 21s)
No errors found when compiling neg test tests/neg/experimentalTests.scala
[================>                       ] completed (852/1928, 11 failed, 22s)
No errors found when compiling neg test tests/neg/experimental-nested-imports.scala
[=================>                      ] completed (889/1928, 12 failed, 23s)
No errors found when compiling neg test tests/neg/14034.scala

No errors found when compiling neg test tests/neg/experimentalAnnot.scala
[=================>                      ] completed (908/1928, 14 failed, 23s)
No errors found when compiling neg test tests/neg/experimentalInline.scala
[==================>                     ] completed (946/1928, 15 failed, 24s)
No errors found when compiling neg test tests/neg/experimentalSignature.scala
[===================>                    ] completed (969/1928, 16 failed, 24s)
No errors found when compiling neg test tests/neg/experimentalInline2.scala
[===================>                    ] completed (996/1928, 17 failed, 25s)
No errors found when compiling neg test tests/neg/experimentalCaseClass.scala
[=====================>                  ] completed (1067/1928, 18 failed, 27s)
No errors found when compiling neg test tests/neg/experimentalMembers.scala

No errors found when compiling neg test tests/neg/tracked2.scala
[=====================>                  ] completed (1082/1928, 20 failed, 27s)
No errors found when compiling neg test tests/neg/experimentalTypeRHS.scala

No errors found when compiling neg test tests/neg/i13091.scala
[=====================>                  ] completed (1094/1928, 22 failed, 27s)
No errors found when compiling neg test tests/neg/i17292.scala
[======================>                 ] completed (1121/1928, 23 failed, 28s)
No errors found when compiling neg test tests/neg/experimentalEnum.scala
[==========================>             ] completed (1348/1928, 24 failed, 33s)
No errors found when compiling neg test tests/neg/experimentalOverride.scala

No errors found when compiling neg test tests/neg/experimental-imports.scala
[===========================>            ] completed (1386/1928, 26 failed, 33s)
No errors found when compiling neg test tests/neg/i17292b.scala
[============================>           ] completed (1427/1928, 27 failed, 34s)
No errors found when compiling neg test tests/neg/use-experimental-def.scala
[=============================>          ] completed (1459/1928, 28 failed, 35s)
No errors found when compiling neg test tests/neg/experimentalRHS.scala
[==============================>         ] completed (1497/1928, 29 failed, 36s)
No errors found when compiling neg test tests/neg/experimentalSam.scala
[==============================>         ] completed (1516/1928, 30 failed, 36s)
No errors found when compiling neg test tests/neg/experimentalTerms.scala
[=================================>      ] completed (1669/1928, 31 failed, 42s)
No errors found when compiling neg test neg/experimental-message-experimental-flag
[=======================================>] completed (1928/1928, 32 failed, 61s)
[error] Test dotty.tools.dotc.CompilationTests.negAll failed: java.lang.AssertionError: Neg test should have failed, but did not, took 61.516 sec
[error]     at dotty.tools.vulpix.ParallelTesting$CompilationTest.checkExpectedErrors(ParallelTesting.scala:1218)
[error]     at dotty.tools.dotc.CompilationTests.negAll(CompilationTests.scala:160)
[error]     at jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
[error]     at jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:77)
[error]     at jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
[error]     at java.lang.reflect.Method.invoke(Method.java:569)
[error]     ...
[info] Test dotty.tools.dotc.CompilationTests.runAll started
[=======================================>] completed (1723/1723, 0 failed, 69s)
[info] Test dotty.tools.dotc.CompilationTests.pickling started
[======>                                 ] completed (893/4754, 0 failed, 10s)-- [E006] Not Found Error: tests/pos/i12953.scala:5:39 -----------------------------------------------------------------
5 |case class Foo[A](@Schema(classOf[List[A]]) foo: String)
  |                                       ^
  |                                       Not found: type A
  |
  | longer explanation available when compiling with `-explain`
-- [E006] Not Found Error: tests/pos/i12953.scala:6:23 -----------------------------------------------------------------
6 |case class Bar[A](@Ann[A] foo: String)
  |                       ^
  |                       Not found: type A
  |
  | longer explanation available when compiling with `-explain`
Compilation failed for: 'tests/pos/i12953.scala'
[=====================>                  ] completed (2655/4754, 1 failed, 31s)Compilation failed for: 'tests/pos/i2997.scala'
-- [E007] Type Mismatch Error: tests/pos/i2997.scala:6:17 --------------------------------------------------------------
6 |  val a : 1 = Foo(1).t
  |              ^^^^^^
  |              Found:    Foo[(1 : scala.Int)]{val t: (1 : scala.Int)}
  |              Required: scala.Selectable | scala.Dynamic
  |
  |              The following import might fix the problem:
  |
  |                import scala.reflect.Selectable.reflectiveSelectable
  |
  |
  | longer explanation available when compiling with `-explain`
[=========================>              ] completed (3119/4754, 2 failed, 36s)Fatal compiler crash when compiling: tests/pos/i20901:
TASTy printer difference for class Foo in tests/pos/i20901/Foo.scala, did not match tests/pos/i20901/Foo.tastycheck,
  output dumped in after-printing.txt, check diff with `git diff --no-index -- tests/pos/i20901/Foo.tastycheck after-printing.txt`
  actual output:
Header:
  version: <elided>
  tooling: <elided>
     UUID: <elided>

Names (351 bytes, starting from <elided base index>):
     0: ASTs
     1: <empty>
     2: scala
     3: reflect
     4: scala[Qualified . reflect]
     5: ClassTag
     6: Foo
     7: <init>
     8: java
     9: lang
    10: java[Qualified . lang]
    11: Object
    12: java[Qualified . lang][Qualified . Object]
    13: <init>[Signed Signature(List(),java.lang.Object) @<init>]
    14: Unit
    15: mkArray
    16: T
    17: Nothing
    18: Any
    19: WitnessNames
    20: annotation
    21: scala[Qualified . annotation]
    22: internal
    23: scala[Qualified . annotation][Qualified . internal]
    24: scala[Qualified . annotation][Qualified . internal][Qualified . WitnessNames]
    25: collection
    26: scala[Qualified . collection]
    27: immutable
    28: scala[Qualified . collection][Qualified . immutable]
    29: Seq
    30: scala[Qualified . collection][Qualified . immutable][Qualified . Seq]
    31: <init>[Signed Signature(List(scala.collection.immutable.Seq),scala.annotation.internal.WitnessNames) @<init>]
    32: String
    33: evidence$1
    34: evidence$
    35: [Unique evidence$ 1]
    36: ???
    37: Predef
    38: SourceFile
    39: scala[Qualified . annotation][Qualified . internal][Qualified . SourceFile]
    40: java[Qualified . lang][Qualified . String]
    41: <init>[Signed Signature(List(java.lang.String),scala.annotation.internal.SourceFile) @<init>]
    42: <elided source file name>
    43: Positions
    44: Comments
    45: Attributes

Trees (122 bytes, starting from <elided base index>):
     0: PACKAGE(120)
     2:   TERMREFpkg 1 [<empty>]
     4:   IMPORT(4)
     6:     TERMREFpkg 4 [scala[Qualified . reflect]]
     8:     IMPORTED 5 [ClassTag]
    10:   TYPEDEF(110) 6 [Foo]
    13:     TEMPLATE(89)
    15:       APPLY(10)
    17:         SELECTin(8) 13 [<init>[Signed Signature(List(),java.lang.Object) @<init>]]
    20:           NEW
    21:             TYPEREF 11 [Object]
    23:               TERMREFpkg 10 [java[Qualified . lang]]
    25:           SHAREDtype 21
    27:       DEFDEF(7) 7 [<init>]
    30:         EMPTYCLAUSE
    31:         TYPEREF 14 [Unit]
    33:           TERMREFpkg 2 [scala]
    35:         STABLE
    36:       DEFDEF(66) 15 [mkArray]
    39:         TYPEPARAM(35) 16 [T]
    42:           TYPEBOUNDStpt(8)
    44:             TYPEREF 17 [Nothing]
    46:               SHAREDtype 33
    48:             TYPEREF 18 [Any]
    50:               SHAREDtype 33
    52:           ANNOTATION(22)
    54:             TYPEREF 19 [WitnessNames]
    56:               TERMREFpkg 23 [scala[Qualified . annotation][Qualified . internal]]
    58:             APPLY(16)
    60:               SELECTin(6) 31 [<init>[Signed Signature(List(scala.collection.immutable.Seq),scala.annotation.internal.WitnessNames) @<init>]]
    63:                 NEW
    64:                   SHAREDtype 54
    66:                 SHAREDtype 54
    68:               REPEATED(6)
    70:                 TYPEREF 32 [String]
    72:                   SHAREDtype 23
    74:                 STRINGconst 33 [evidence$1]
    76:         PARAM(14) 35 [[Unique evidence$ 1]]
    79:           APPLIEDtpt(10)
    81:             IDENTtpt 5 [ClassTag]
    83:               TYPEREF 5 [ClassTag]
    85:                 SHAREDtype 6
    87:             IDENTtpt 16 [T]
    89:               TYPEREFdirect 39
    91:           GIVEN
    92:         IDENTtpt 17 [Nothing]
    94:           TYPEREF 17 [Nothing]
    96:             TERMREFpkg 2 [scala]
    98:         TERMREF 36 [???]
   100:           TERMREF 37 [Predef]
   102:             SHAREDtype 33
   104:     ANNOTATION(16)
   106:       TYPEREF 38 [SourceFile]
   108:         SHAREDtype 56
   110:       APPLY(10)
   112:         SELECTin(6) 41 [<init>[Signed Signature(List(java.lang.String),scala.annotation.internal.SourceFile) @<init>]]
   115:           NEW
   116:             SHAREDtype 106
   118:           SHAREDtype 106
   120:         STRINGconst 42 [<elided source file name>]
   122:

Positions (83 bytes, starting from <elided base index>):
  lines: 7
  line sizes:
     38, 0, 23, 0, 10, 41, 0
  positions:
     0: 40 .. 117
     4: 40 .. 63
     6: 47 .. 54
     8: 55 .. 63
    10: 65 .. 117
    13: 78 .. 117
    21: 71 .. 71
    27: 78 .. 78
    31: 78 .. 78
    36: 78 .. 117
    39: 90 .. 101
    44: 93 .. 93
    48: 93 .. 93
    64: 90 .. 90
    70: 90 .. 90
    74: 90 .. 90
    76: 93 .. 101
    81: 93 .. 101
    87: 93 .. 101
    92: 104 .. 111
    98: 114 .. 117
   110: 65 .. 117
   116: 65 .. 65
   120: 65 .. 65

  source paths:
     0: 42 [<elided source file name>]

Attributes (2 bytes, starting from <elided base index>):
  SOURCEFILEattr 42 [<elided source file name>]
	at dotty.tools.dotc.reporting.ThrowingReporter.doReport(ThrowingReporter.scala:14)
	at dotty.tools.dotc.reporting.Reporter.issueUnconfigured(Reporter.scala:160)
	at dotty.tools.dotc.reporting.Reporter.go$1(Reporter.scala:191)
	at dotty.tools.dotc.reporting.Reporter.issueIfNotSuppressed(Reporter.scala:210)
	at dotty.tools.dotc.reporting.Reporter.report(Reporter.scala:213)
	at dotty.tools.dotc.report$.error(report.scala:69)
	at dotty.tools.dotc.transform.Pickler.testSamePrinted$$anonfun$1(Pickler.scala:482)
	at scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	at scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	at scala.Option.foreach(Option.scala:437)
	at dotty.tools.dotc.transform.Pickler.testSamePrinted(Pickler.scala:477)
	at dotty.tools.dotc.transform.Pickler.testUnpickler$$anonfun$2(Pickler.scala:456)
	at scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	at scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	at scala.collection.IterableOnceOps.foreach(IterableOnce.scala:619)
	at scala.collection.IterableOnceOps.foreach$(IterableOnce.scala:617)
	at scala.collection.AbstractIterable.foreach(Iterable.scala:935)
	at scala.collection.IterableOps$WithFilter.foreach(Iterable.scala:905)
	at dotty.tools.dotc.transform.Pickler.testUnpickler(Pickler.scala:442)
	at dotty.tools.dotc.transform.Pickler.runOn(Pickler.scala:412)
	at dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:367)
	at scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	at scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	at scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1324)
	at dotty.tools.dotc.Run.runPhases$1(Run.scala:360)
	at dotty.tools.dotc.Run.compileUnits$$anonfun$1$$anonfun$2(Run.scala:407)
	at dotty.tools.dotc.Run.compileUnits$$anonfun$1$$anonfun$adapted$1(Run.scala:407)
	at scala.Function0.apply$mcV$sp(Function0.scala:42)
	at dotty.tools.dotc.Run.showProgress(Run.scala:469)
	at dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:407)
	at dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:419)
	at dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:69)
	at dotty.tools.dotc.Run.compileUnits(Run.scala:419)
	at dotty.tools.dotc.Run.compileSources(Run.scala:306)
	at dotty.tools.dotc.Run.compile(Run.scala:291)
	at dotty.tools.dotc.Driver.doCompile(Driver.scala:37)
	at dotty.tools.dotc.Driver.process(Driver.scala:201)
	at dotty.tools.dotc.Driver.process(Driver.scala:169)
	at dotty.tools.vulpix.ParallelTesting$Test.compile(ParallelTesting.scala:564)
	at dotty.tools.vulpix.ParallelTesting$CompilationLogic.compileTestSource$$anonfun$1$$anonfun$1(ParallelTesting.scala:261)
	at scala.collection.immutable.List.map(List.scala:247)
	at dotty.tools.vulpix.ParallelTesting$CompilationLogic.compileTestSource$$anonfun$1(ParallelTesting.scala:259)
	at scala.util.Try$.apply(Try.scala:217)
	at dotty.tools.vulpix.ParallelTesting$CompilationLogic.dotty$tools$vulpix$ParallelTesting$CompilationLogic$$compileTestSource(ParallelTesting.scala:264)
	at dotty.tools.vulpix.ParallelTesting$$anon$3.checkTestSource$$anonfun$1(ParallelTesting.scala:295)
	at dotty.tools.vulpix.ParallelTesting$$anon$3.checkTestSource$$anonfun$adapted$1(ParallelTesting.scala:298)
	at scala.Function0.apply$mcV$sp(Function0.scala:42)
	at dotty.tools.vulpix.ParallelTesting$Test.tryCompile(ParallelTesting.scala:482)
	at dotty.tools.vulpix.ParallelTesting$$anon$3.checkTestSource(ParallelTesting.scala:298)
	at dotty.tools.vulpix.ParallelTesting$Test$LoggedRunnable.run(ParallelTesting.scala:378)
	at dotty.tools.vulpix.ParallelTesting$Test$LoggedRunnable.run$(ParallelTesting.scala:360)
	at dotty.tools.vulpix.ParallelTesting$$anon$3.run(ParallelTesting.scala:293)
	at java.base/java.util.concurrent.ForkJoinTask$AdaptedRunnableAction.exec(ForkJoinTask.java:1375)
	at java.base/java.util.concurrent.ForkJoinTask.doExec(ForkJoinTask.java:373)
	at java.base/java.util.concurrent.ForkJoinPool$WorkQueue.topLevelExec(ForkJoinPool.java:1182)
	at java.base/java.util.concurrent.ForkJoinPool.scan(ForkJoinPool.java:1655)
	at java.base/java.util.concurrent.ForkJoinPool.runWorker(ForkJoinPool.java:1622)
	at java.base/java.util.concurrent.ForkJoinWorkerThread.run(ForkJoinWorkerThread.java:165)
[=======================================>] completed (4754/4754, 3 failed, 76s)
[error] Test dotty.tools.dotc.CompilationTests.pickling failed: java.lang.AssertionError: Pos test failed, but should not, reasons:
[error] encountered 3 test failure(s):
[error]   - generic failure (see test output), took 77.724 sec
[error]     at dotty.tools.vulpix.ParallelTesting$CompilationTest.checkPass(ParallelTesting.scala:1269)
[error]     at dotty.tools.vulpix.ParallelTesting$CompilationTest.checkCompile(ParallelTesting.scala:1185)
[error]     at dotty.tools.dotc.CompilationTests.pickling(CompilationTests.scala:195)
[error]     at jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
[error]     at jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:77)
[error]     at jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
[error]     at java.lang.reflect.Method.invoke(Method.java:569)
[error]     ...
[info] Test dotty.tools.dotc.CompilationTests.rewrites started
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (24/24, 0 failed, 1s)
[info] Test dotty.tools.dotc.CompilationTests.pos started
[==>                                     ] completed (273/3538, 0 failed, 6s)-- [E006] Not Found Error: tests/pos/i12953.scala:5:39 -----------------------------------------------------------------
5 |case class Foo[A](@Schema(classOf[List[A]]) foo: String)
  |                                       ^
  |                                       Not found: type A
  |
  | longer explanation available when compiling with `-explain`
-- [E006] Not Found Error: tests/pos/i12953.scala:6:23 -----------------------------------------------------------------
6 |case class Bar[A](@Ann[A] foo: String)
  |                       ^
  |                       Not found: type A
  |
  | longer explanation available when compiling with `-explain`
Compilation failed for: 'tests/pos/i12953.scala'
[========>                               ] completed (825/3538, 1 failed, 16s)-- [E007] Type Mismatch Error: tests/pos/i2997.scala:6:17 --------------------------------------------------------------
6 |  val a : 1 = Foo(1).t
  |              ^^^^^^
  |              Found:    Foo[(1 : Int)]{val t: (1 : Int)}
  |              Required: Selectable | Dynamic
  |
  |              The following import might fix the problem:
  |
  |                import scala.reflect.Selectable.reflectiveSelectable
  |
  |
  | longer explanation available when compiling with `-explain`
Compilation failed for: 'tests/pos/i2997.scala'
[=======================================>] completed (3538/3538, 2 failed, 92s)
[error] Test dotty.tools.dotc.CompilationTests.pos failed: java.lang.AssertionError: Pos test failed, but should not, reasons:
[error] encountered 2 test failure(s):
[error]   - generic failure (see test output), took 93.413 sec
[error]     at dotty.tools.vulpix.ParallelTesting$CompilationTest.checkPass(ParallelTesting.scala:1269)
[error]     at dotty.tools.vulpix.ParallelTesting$CompilationTest.checkCompile(ParallelTesting.scala:1185)
[error]     at dotty.tools.dotc.CompilationTests.pos(CompilationTests.scala:55)
[error]     at jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
[error]     at jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:77)
[error]     at jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
[error]     at java.lang.reflect.Method.invoke(Method.java:569)
[error]     ...
[info] Test dotty.tools.dotc.CompilationTests.warn started
[=======================================>] completed (275/275, 0 failed, 5s)
[info] Test dotty.tools.dotc.CompilationTests.posCC started
[=======================================>] completed (149/149, 0 failed, 2s)
[info] Test dotty.tools.dotc.CompilationTests.checkInit started
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (149/149, 0 failed, 1s)
[=======================================>] completed (59/59, 0 failed, 0s)
[=======================================>] completed (25/25, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[=======================================>] completed (1/1, 0 failed, 0s)
[info] Test dotty.tools.dotc.CompilationTests.posTwice started
[=======================================>] completed (133/133, 0 failed, 11s)
[info] Test dotty.tools.dotc.CompilationTests.explicitNullsNeg started
[=======================================>] completed (73/73, 0 failed, 3s)
[info] Test dotty.tools.dotc.CompilationTests.explicitNullsPos started
[=======================================>] completed (98/98, 0 failed, 5s)
[info] Test dotty.tools.dotc.CompilationTests.explicitNullsRun started
[=======================================>] completed (12/12, 0 failed, 0s)
[info] Test dotty.tools.dotc.CompilationTests.checkInitGlobal started
[=======================================>] completed (56/56, 0 failed, 0s)
[=======================================>] completed (48/48, 0 failed, 0s)
[info] Test dotty.tools.dotc.CompilationTests.fuzzyAll started
[=======================================>] completed (184/184, 0 failed, 2s)
[info] Test dotty.tools.dotc.CompilationTests.genericJavaSignatures started
[=======================================>] completed (26/26, 0 failed, 0s)
[info] Test dotty.tools.dotc.CompilationTests.explicitNullsWarn started
[=======================================>] completed (2/2, 0 failed, 0s)

================================================================================
Test Report
================================================================================

51 suites passed, 3 failed, 54 total
    tests/neg/experimentalEnum.scala failed
    tests/neg/experimental.scala failed
    tests/neg/experimentalSam.scala failed
    tests/neg/experimentalRHS.scala failed
    tests/neg/experimental-nested-imports.scala failed
    tests/neg/use-experimental-def.scala failed
    tests/neg/experimentalDefaultParams.scala failed
    tests/neg/i17292b.scala failed
    tests/neg/experimental-message-experimental-flag failed
    tests/neg/tracked2.scala failed
    tests/neg/experimentalUnapply.scala failed
    tests/neg/14034.scala failed
    tests/neg/experimental-nested-imports-2.scala failed
    tests/neg/experimentalSignature.scala failed
    tests/neg/experimental-imports.scala failed
    tests/neg/i13091.scala failed
    tests/neg/i17292.scala failed
    tests/neg/experimental-nested-imports-3.scala failed
    tests/neg/i13848.scala failed
    tests/neg/experimentalMembers.scala failed
    tests/neg/experimentalType.scala failed
    tests/neg/experimentalTypeRHS.scala failed
    tests/neg/experimentalTests.scala failed
    tests/neg/experimentalInline2.scala failed
    tests/neg/experimentalTerms.scala failed
    tests/neg/experimentalTypes2.scala failed
    tests/neg/experimentalAnnot.scala failed
    tests/neg/experimentalCaseClass.scala failed
    tests/neg/experimentalOverride.scala failed
    tests/neg/expeimental-flag.scala failed
    tests/neg/experimentalInline.scala failed
    tests/neg/experimental-message.scala failed
    tests/pos/i12953.scala failed
    tests/pos/i2997.scala failed
    tests/pos/i20901 failed
    tests/pos/i12953.scala failed
    tests/pos/i2997.scala failed

--------------------------------------------------------------------------------
Note - reproduction instructions have been dumped to log file:
    /home/kpi/scala3/testlogs/tests-2025-04-14/tests-2025-04-14-T09-36-43.log
--------------------------------------------------------------------------------
[info] Test run dotty.tools.dotc.CompilationTests finished: 3 failed, 0 ignored, 17 total, 364.817s
[info] Test run dotty.tools.dotc.BootstrappedOnlyCompilationTests started
[info] Test run dotty.tools.dotc.BootstrappedOnlyCompilationTests finished: 0 failed, 0 ignored, 0 total, 0.0s
[info] Test run dotty.tools.dotc.coverage.CoverageTests started
[info] Test run dotty.tools.dotc.coverage.CoverageTests finished: 0 failed, 0 ignored, 0 total, 0.0s
[error] Failed: Total 17, Failed 3, Errors 0, Passed 14
[error] Failed tests:
[error] 	dotty.tools.dotc.CompilationTests
[error] (scala3-compiler / Test / testOnly) sbt.TestsFailedException: Tests unsuccessful
[error] Total time: 367 s (06:07), completed Apr 14, 2025, 9:40:05 AM
