# Simple Tracked Inference Failures

## Failed tests:
- tests/neg/experimentalEnum.scala failed
- tests/neg/experimental.scala failed
- tests/neg/experimentalSam.scala failed
- tests/neg/experimentalRHS.scala failed
- tests/neg/experimental-nested-imports.scala failed
- tests/neg/use-experimental-def.scala failed
- tests/neg/experimentalDefaultParams.scala failed
- tests/neg/i17292b.scala failed
- tests/neg/experimental-message-experimental-flag failed
- tests/neg/tracked2.scala failed
- tests/neg/experimentalUnapply.scala failed
- tests/neg/14034.scala failed
- tests/neg/experimental-nested-imports-2.scala failed
- tests/neg/experimentalSignature.scala failed
- tests/neg/experimental-imports.scala failed
- tests/neg/i13091.scala failed
- tests/neg/i17292.scala failed
- tests/neg/experimental-nested-imports-3.scala failed
- tests/neg/i13848.scala failed
- tests/neg/experimentalMembers.scala failed
- tests/neg/experimentalType.scala failed
- tests/neg/experimentalTypeRHS.scala failed
- tests/neg/experimentalTests.scala failed
- tests/neg/experimentalInline2.scala failed
- tests/neg/experimentalTerms.scala failed
- tests/neg/experimentalTypes2.scala failed
- tests/neg/experimentalAnnot.scala failed
- tests/neg/experimentalCaseClass.scala failed
- tests/neg/experimentalOverride.scala failed
- tests/neg/expeimental-flag.scala failed
- tests/neg/experimentalInline.scala failed
- tests/neg/experimental-message.scala failed
- tests/pos/i12953.scala failed
- tests/pos/i2997.scala failed
- tests/pos/i20901 failed
- tests/pos/i12953.scala failed
- tests/pos/i2997.scala failed

## Non-experimental related tests:
- tests/pos/i12953.scala failed
- tests/pos/i2997.scala failed
- tests/pos/i20901 failed

### One TASTY change, which is expected. Unrelated to tracked, only to `@WitnessNames` annotations AFAIK:
- tests/pos/i20901 failed

source:
```scala
//> using options -Ytest-pickler-check

import reflect.ClassTag

class Foo:
  def mkArray[T: ClassTag]: Nothing = ???
```

diff
```scala
diff --git a/tests/pos/i20901/Foo.tastycheck b/after-printing.txt
index 82e95946f9..90cfcf8e95 100644
--- a/tests/pos/i20901/Foo.tastycheck
+++ b/after-printing.txt
@@ -3,7 +3,7 @@ Header:
   tooling: <elided>
      UUID: <elided>

-Names (276 bytes, starting from <elided base index>):
+Names (351 bytes, starting from <elided base index>):
      0: ASTs
      1: <empty>
      2: scala
@@ -23,32 +23,42 @@ Names (276 bytes, starting from <elided base index>):
     16: T
     17: Nothing
     18: Any
-    19: evidence$
-    20: [Unique evidence$ 1]
-    21: ???
-    22: Predef
-    23: SourceFile
-    24: annotation
-    25: scala[Qualified . annotation]
-    26: internal
-    27: scala[Qualified . annotation][Qualified . internal]
-    28: scala[Qualified . annotation][Qualified . internal][Qualified . SourceFile]
-    29: String
-    30: java[Qualified . lang][Qualified . String]
-    31: <init>[Signed Signature(List(java.lang.String),scala.annotation.internal.SourceFile) @<init>]
-    32: <elided source file name>
-    33: Positions
-    34: Comments
-    35: Attributes
+    19: WitnessNames
+    20: annotation
+    21: scala[Qualified . annotation]
+    22: internal
+    23: scala[Qualified . annotation][Qualified . internal]
+    24: scala[Qualified . annotation][Qualified . internal][Qualified . WitnessNames]
+    25: collection
+    26: scala[Qualified . collection]
+    27: immutable
+    28: scala[Qualified . collection][Qualified . immutable]
+    29: Seq
+    30: scala[Qualified . collection][Qualified . immutable][Qualified . Seq]
+    31: <init>[Signed Signature(List(scala.collection.immutable.Seq),scala.annotation.internal.WitnessNames) @<init>]
+    32: String
+    33: evidence$1
+    34: evidence$
+    35: [Unique evidence$ 1]
+    36: ???
+    37: Predef
+    38: SourceFile
+    39: scala[Qualified . annotation][Qualified . internal][Qualified . SourceFile]
+    40: java[Qualified . lang][Qualified . String]
+    41: <init>[Signed Signature(List(java.lang.String),scala.annotation.internal.SourceFile) @<init>]
+    42: <elided source file name>
+    43: Positions
+    44: Comments
+    45: Attributes

-Trees (98 bytes, starting from <elided base index>):
-     0: PACKAGE(96)
+Trees (122 bytes, starting from <elided base index>):
+     0: PACKAGE(120)
      2:   TERMREFpkg 1 [<empty>]
      4:   IMPORT(4)
      6:     TERMREFpkg 4 [scala[Qualified . reflect]]
      8:     IMPORTED 5 [ClassTag]
-    10:   TYPEDEF(86) 6 [Foo]
-    13:     TEMPLATE(65)
+    10:   TYPEDEF(110) 6 [Foo]
+    13:     TEMPLATE(89)
     15:       APPLY(10)
     17:         SELECTin(8) 13 [<init>[Signed Signature(List(),java.lang.Object) @<init>]]
     20:           NEW
@@ -60,39 +70,51 @@ Trees (98 bytes, starting from <elided base index>):
     31:         TYPEREF 14 [Unit]
     33:           TERMREFpkg 2 [scala]
     35:         STABLE
-    36:       DEFDEF(42) 15 [mkArray]
-    39:         TYPEPARAM(11) 16 [T]
+    36:       DEFDEF(66) 15 [mkArray]
+    39:         TYPEPARAM(35) 16 [T]
     42:           TYPEBOUNDStpt(8)
     44:             TYPEREF 17 [Nothing]
     46:               SHAREDtype 33
     48:             TYPEREF 18 [Any]
     50:               SHAREDtype 33
-    52:         PARAM(14) 20 [[Unique evidence$ 1]]
-    55:           APPLIEDtpt(10)
-    57:             IDENTtpt 5 [ClassTag]
-    59:               TYPEREF 5 [ClassTag]
-    61:                 SHAREDtype 6
-    63:             IDENTtpt 16 [T]
-    65:               TYPEREFdirect 39
-    67:           GIVEN
-    68:         IDENTtpt 17 [Nothing]
-    70:           TYPEREF 17 [Nothing]
-    72:             TERMREFpkg 2 [scala]
-    74:         TERMREF 21 [???]
-    76:           TERMREF 22 [Predef]
-    78:             SHAREDtype 33
-    80:     ANNOTATION(16)
-    82:       TYPEREF 23 [SourceFile]
-    84:         TERMREFpkg 27 [scala[Qualified . annotation][Qualified . internal]]
-    86:       APPLY(10)
-    88:         SELECTin(6) 31 [<init>[Signed Signature(List(java.lang.String),scala.annotation.internal.SourceFile) @<init>]]
-    91:           NEW
-    92:             SHAREDtype 82
-    94:           SHAREDtype 82
-    96:         STRINGconst 32 [<elided source file name>]
-    98:
+    52:           ANNOTATION(22)
+    54:             TYPEREF 19 [WitnessNames]
+    56:               TERMREFpkg 23 [scala[Qualified . annotation][Qualified . internal]]
+    58:             APPLY(16)
+    60:               SELECTin(6) 31 [<init>[Signed Signature(List(scala.collection.immutable.Seq),scala.annotation.internal.WitnessNames) @<init>]]
+    63:                 NEW
+    64:                   SHAREDtype 54
+    66:                 SHAREDtype 54
+    68:               REPEATED(6)
+    70:                 TYPEREF 32 [String]
+    72:                   SHAREDtype 23
+    74:                 STRINGconst 33 [evidence$1]
+    76:         PARAM(14) 35 [[Unique evidence$ 1]]
+    79:           APPLIEDtpt(10)
+    81:             IDENTtpt 5 [ClassTag]
+    83:               TYPEREF 5 [ClassTag]
+    85:                 SHAREDtype 6
+    87:             IDENTtpt 16 [T]
+    89:               TYPEREFdirect 39
+    91:           GIVEN
+    92:         IDENTtpt 17 [Nothing]
+    94:           TYPEREF 17 [Nothing]
+    96:             TERMREFpkg 2 [scala]
+    98:         TERMREF 36 [???]
+   100:           TERMREF 37 [Predef]
+   102:             SHAREDtype 33
+   104:     ANNOTATION(16)
+   106:       TYPEREF 38 [SourceFile]
+   108:         SHAREDtype 56
+   110:       APPLY(10)
+   112:         SELECTin(6) 41 [<init>[Signed Signature(List(java.lang.String),scala.annotation.internal.SourceFile) @<init>]]
+   115:           NEW
+   116:             SHAREDtype 106
+   118:           SHAREDtype 106
+   120:         STRINGconst 42 [<elided source file name>]
+   122:

-Positions (75 bytes, starting from <elided base index>):
+Positions (83 bytes, starting from <elided base index>):
   lines: 7
   line sizes:
      38, 0, 23, 0, 10, 41, 0
@@ -110,17 +132,20 @@ Positions (75 bytes, starting from <elided base index>):
     39: 90 .. 101
     44: 93 .. 93
     48: 93 .. 93
-    52: 93 .. 101
-    57: 93 .. 101
-    63: 93 .. 101
-    68: 104 .. 111
-    74: 114 .. 117
-    86: 65 .. 117
-    92: 65 .. 65
-    96: 65 .. 65
+    64: 90 .. 90
+    70: 90 .. 90
+    74: 90 .. 90
+    76: 93 .. 101
+    81: 93 .. 101
+    87: 93 .. 101
+    92: 104 .. 111
+    98: 114 .. 117
+   110: 65 .. 117
+   116: 65 .. 65
+   120: 65 .. 65

   source paths:
-     0: 32 [<elided source file name>]
+     0: 42 [<elided source file name>]

 Attributes (2 bytes, starting from <elided base index>):
-  SOURCEFILEattr 32 [<elided source file name>]
+  SOURCEFILEattr 42 [<elided source file name>]
```

### One problem with typing annotations too early:
- tests/pos/i12953.scala

source:
```scala
class Schema(impl: Class[_]) extends scala.annotation.StaticAnnotation

class Ann[A] extends scala.annotation.StaticAnnotation

case class Foo[A](@Schema(classOf[List[A]]) foo: String)
case class Bar[A](@Ann[A] foo: String)
def baz[A](@Ann[A] foo: String) = ()
```

error:
```scala
-- [E006] Not Found Error: tests/pos/i12953.scala:5:39 -------------------------
5 |case class Foo[A](@Schema(classOf[List[A]]) foo: String)
  |                                       ^
  |                                       Not found: type A
  |
  | longer explanation available when compiling with `-explain`
-- [E006] Not Found Error: tests/pos/i12953.scala:6:23 -------------------------
6 |case class Bar[A](@Ann[A] foo: String)
  |                       ^
  |                       Not found: type A
  |
  | longer explanation available when compiling with `-explain`
-- Warning: tests/pos/i12953.scala:1:25 ----------------------------------------
1 |class Schema(impl: Class[_]) extends scala.annotation.StaticAnnotation
  |                         ^
  |`_` is deprecated for wildcard arguments of types: use `?` instead
  |This construct can be rewritten automatically under -rewrite -source 3.4-migration.
```

### One type checking error involving Selectable:
- tests/pos/i2997.scala

test:
```scala
case class Foo[T <: Int with Singleton](t : T)

object Test {
  val one = 1
  final val final_one = 1
  val a : 1 = Foo(1).t
  val b : one.type = Foo(one).t
  val c : 1 = Foo(final_one).t
}
```

error:
```scala
-- [E007] Type Mismatch Error: tests/pos/i2997.scala:6:17 ----------------------
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
```

`tracked`-less reproduction:
```scala
case class Foo(t : Int)

object Foo {
  def apply(t0: Int) : Foo {val t : t0.type} = ???
}

object Test {
  val a : 1 = Foo(1).t
}
```