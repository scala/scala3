package dotty.tools.repl

import org.junit.Test
import org.junit.Assert.assertEquals
import scala.annotation.tailrec
import dotty.tools.dotc.core.StdNames.{nme, str}

abstract class ReplDisassemblerTest extends ReplTest:
  def packageSeparator: String = "."
  def eval(code: String): State =
    val state = initially { run(code) }
    val _ = storedOutput()  // discard output
    state
  def line(n: Int): String = s"${str.REPL_SESSION_LINE}$n"
  def line(n: Int, rest: String): String = line(n) + "$" + rest

// Test option parsing
class DisassemblerOptionParsingTests extends ReplDisassemblerTest:
  private val helps = List(
    "usage"       -> ":tool [opts] [path or class or -]...",
    "-verbose/-v" -> "Stack size, number of locals, method args",
    "-private/-p" -> "Private classes and members",
    "-protected"  -> "Protected classes and members",
    "-s"          -> "Internal type signatures",
    "-sysinfo"    -> "System info of class",
    "-filter"     -> "Filter REPL machinery from output",
  )
  private val options = new DisassemblerOptionParser(helps):
    val defaultToolOptions = List("-protected", "-verbose")

  private def parsed(input: Seq[String])(using state: State): DisassemblerOptions =
    given DisassemblerRepl(this, state)
    options.parse(input)

  private def assertTargets(expected: Seq[String], input: Seq[String])(using State) =
    assertEquals(expected, parsed(input).targets)

  private def assertFlags(expected: Seq[String], input: Seq[String])(using State) =
    assertEquals(expected, parsed(input).flags)

  private def assertFilter(expected: Boolean, input: Seq[String])(using State) =
    assertEquals(expected, parsed(input).filterReplNames)

  // disassembly targets are extracted properly
  @Test def targets =
    eval("class XYZ").andThen {
      assertTargets(Nil, Nil)
      assertTargets(Seq("t1", "t2#m"), Seq("t1", "t2#m"))
      assertTargets(Seq(line(1, "XYZ"), "/tmp/t2.class"), Seq("-pro", "-filter", "-verb", "-", "/tmp/t2.class"))
      assertTargets(Seq("t1.class", "scala.util.Either#orElse"), Seq("-s", "t1.class", "-filter", "-pri", "scala.util.Either#orElse"))
    }

  // normal flags are set properly
  @Test def flags =
    initially {
      assertFlags(options.defaultToolOptions, Nil)
      assertFlags(Seq("-p"), Seq("-p", "target"))
      assertFlags(Seq("-private"), Seq("-private", "target"))
      assertFlags(Seq("-sysinfo", "-v"), Seq("-sys", "-v", "target"))
      assertFlags(Seq("-protected", "-verbose"), Seq("-pro", "-filter", "-verb", "t1", "t2"))
      assertFlags(Seq("-s", "-private"), Seq("-s", "t1", "-filter", "-pri", "t2"))
    }

  // unrecognized flags should result in -help
  @Test def flagsUnknown =
    initially {
      assertFlags(Seq("-help"), Seq("-unknown", "target"))
    }

  // the special `-filter` flag is extracted and set properly
  @Test def optionFilter =
    initially {
      assertFilter(false, Nil)
      assertFilter(false, Seq("-p", "-v", "target"))
      assertFilter(false, Seq("-", "filter", "target"))
      assertFilter(true, Seq("-filter", "target"))
      assertFilter(true, Seq("-filt", "target"))
      assertFilter(true, Seq("-p", "-f", "-v", "target"))
    }
end DisassemblerOptionParsingTests

// Test target resolution
// TODO expand on what that means
class DisassemblyTargetTests extends ReplDisassemblerTest:
  import DisassemblyTool.Input

  private def assertTargets(expected: Seq[String], targets: Seq[String])(using state: State): Unit =
    given repl: DisassemblerRepl = DisassemblerRepl(this, state)
    val clazz = DisassemblyClass(repl.classLoader)
    val targetNames = targets.flatMap(t => if t == "-" then repl.mostRecentEntry else Seq(t))
    assertEquals(expected, targetNames.map(clazz.bytes(_).actual))

  private def assertTarget(expected: String, target: String)(using State): Unit =
    assertTargets(expected :: Nil, target :: Nil)

  @Test def targetOfStdlib =
    initially {
      assertTarget("scala.collection.immutable.List", "List")
      assertTarget("scala.collection.immutable.List$", "List$")
      assertTarget("scala.collection.immutable.List$", "List.type")
      assertTarget("scala.collection.immutable.List", "List#tail")
    }

  @Test def targetOfJavaStdlib =
    initially {
      assertTarget("java.lang.String", "String#substring")
      assertTarget("java.lang.Character$UnicodeBlock", "Character.UnicodeBlock")
    }

  @Test def targetOfEmptyClass =
    eval("class C").andThen {
      assertTarget(line(1, "C"), "C")
    }

  @Test def targetOfEnum =
    eval(
      """enum Color {
        |  case Red, Green, Blue
        |}
      """.stripMargin
    ).andThen {
      assertTarget(line(1, "Color"), "Color")
      assertTarget(line(1, "Color$"), "Color$")
      assertTarget(line(1, "Color$"), "Color.type")
      assertTarget(line(1, "Color$"), "Color.Green")
    }

  @Test def targetOfClassInsideClass =
    eval(
      """class Greeting {
        |  class D {
        |    def howdy() = println("howdy")
        |  }
        |}
      """.stripMargin
    ).andThen {
      assertTarget(line(1, "Greeting"), "Greeting")
      assertTarget(line(1, "Greeting$D"), "Greeting$D")    // resolved by desynthesize()
      //assertTarget(line(1, "Greeting$D"), "Greeting#D")  // XXX fails, triggers filtering instead of inner class selection
    }

  @Test def targetOfClassInsideObject =
    eval(
      """object Hello {
        |  class C {
        |    def hello() = println("hi")
        |  }
        |}
      """.stripMargin
    ).andThen {
      assertTarget(line(1, "Hello$"), "Hello")
      assertTarget(line(1, "Hello$C"), "Hello.C")
      //assertTarget(line(1, "Hello$C"), "Hello$C")  // XXX fails to resolve, desynthesize expects prefix to be a type
    }

  @Test def targetOfTypeConstructor =
    eval(
      """class Id[A] {
        |  def me(in: A): A = in
        |}
      """.stripMargin
    ).andThen {
      assertTarget(line(1, "Id"), "Id")
      assertTarget(line(1, "Id"), "Id[?]")
      assertTarget(line(1, "Id"), "Id#me")
    }

  @Test def targetOfTypeAlias =
    eval(
      """class IntAdder {
        |  def addOne(x: Int) = x + 1
        |}
        |type IA = IntAdder
      """.stripMargin
    ).andThen {
      assertTarget(line(1, "IntAdder"), "IA")
      assertTarget(line(1, "IntAdder"), "IA#addOne")
    }

  @Test def targetOfTargetNameClass =
    eval(
      """import scala.annotation.targetName
        |@targetName("Target") class Defined {
        |  def self: Defined = this
        |}
      """.stripMargin
    ).andThen {
      assertTarget(line(1, "Target"), "Defined")
      assertTarget("Target", "Target")  // fall back to verbatim requested target
    }

  @Test def targetOfSimpleVal =
    eval("val x = 42").andThen {
      assertTarget(line(1, ""), "x")
    }

  @Test def targetOfNullaryMethod =
    eval("def nonRandom() = 42").andThen {
      assertTarget(line(1, ""), "nonRandom()")
    }

  @Test def targetOfJavaStaticVal =
    initially {
      assertTarget("java.time.DayOfWeek", "java.time.DayOfWeek.MONDAY")
    }

  @Test def targetOfLast =
    eval("class C").andThen { assertTarget(line(1, "C"), "-") }
    eval("object X").andThen { assertTarget(line(1, "X$"), "-") }
    eval("val t = 0").andThen { assertTarget(line(1, ""), "-") }
    eval("def f = 10").andThen { assertTarget(line(1, ""), "-") }
    eval(
      """class C
        |class D
      """.stripMargin
    ).andThen {
      assertTargets(List(line(1, "C"), line(1, "D")), Seq("-"))
    }
    eval(
      """import scala.annotation.targetName
        |@targetName("Target") class Defined
      """.stripMargin
    ).andThen {
      assertTarget(line(1, "Target"), "-")
    }
end DisassemblyTargetTests

abstract class DisassemblerTest extends ReplDisassemblerTest:
  def assertDisassemblyIncludes(line: String, output: String | Null = null): Unit =
    val out = if output == null then storedOutput() else output
    assert(out.linesIterator.exists(_.contains(line)),
      s"disassembly did not contain `$line`\nDisassembly was:\n$out")

  // NB: supplied expected lines must occur in the same order in the output
  def assertDisassemblyIncludes(lines: List[String]): Unit =
    val out = storedOutput()
    @tailrec def loop(input: Iterator[String], expected: List[String]): Unit =
      expected match
        case Nil =>
        case x :: xs =>
          val it = input.dropWhile(!_.contains(x))
          assert(it.hasNext, s"disassembly did not contain `$x`\nDisassembly was:\n$out")
          loop(it.drop(1), xs)
    loop(out.linesIterator, lines)

  def assertDisassemblyExcludes(line: String, output: String | Null = null): Unit =
    val out = if output == null then storedOutput() else output
    assert(!out.linesIterator.exists(_.contains(line)),
      s"disassembly unexpectedly contained `$line`")
end DisassemblerTest

// Test disassembly using `:javap`
class JavapTests extends DisassemblerTest:
  override val packageSeparator = "."

  @Test def `simple end-to-end` =
    eval("class Foo1").andThen {
      run(":javap -c Foo1")
      assertDisassemblyIncludes(s"public class ${line(1, "Foo1")} {")
    }

  @Test def `multiple classes in prev entry` =
    eval {
      """class Foo2
        |trait Bar2
        |""".stripMargin
    } andThen {
      run(":javap -c -")
      assertDisassemblyIncludes(List(
        s"public class ${line(1, "Foo2")} {",
        s"public interface ${line(1, "Bar2")} {",
      ))
    }

  @Test def `private selected method` =
    eval {
      """class Baz1:
        |  private def one = 1
        |  private def two = 2
        |""".stripMargin
    } andThen {
      run(":javap -p -c Baz1#one")
      val out = storedOutput()
      assertDisassemblyIncludes("private int one();", out)
      assertDisassemblyExcludes("private int two();", out)
    }

  @Test def `java.lang.String signatures` =
    initially {
      run(":javap -s java.lang.String")
      val out = storedOutput()
      assertDisassemblyIncludes("public static java.lang.String format(java.lang.String, java.lang.Object...);", out)
      assertDisassemblyIncludes("public static java.lang.String join(java.lang.CharSequence, java.lang.Iterable<? extends java.lang.CharSequence>);", out)
      assertDisassemblyIncludes("public java.lang.String concat(java.lang.String);", out)
      assertDisassemblyIncludes("public java.lang.String trim();", out)
    }
end JavapTests

// Test output filters
class JavapFilterSelectionTests:
  // test -sysinfo disassembly
  private val listSysinfo =
    """  Size 51190 bytes
      |  MD5 checksum fa1f9a810f5fff1bac4c3d1ae2051ab5
      |  Compiled from "List.scala"
      |public abstract class scala.collection.immutable.List<A> extends scala.collection.immutable.AbstractSeq<A> implements scala.collection.immutable.LinearSeq<A>, scala.collection.StrictOptimizedLinearSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.immutable.StrictOptimizedSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.generic.DefaultSerializable {
      |  public static scala.collection.SeqOps unapplySeq(scala.collection.SeqOps);
      |  public scala.collection.LinearSeq drop(int);
      |  public scala.collection.LinearSeq dropWhile(scala.Function1);
      |  public java.lang.Object drop(int);
      |  public java.lang.Object sorted(scala.math.Ordering);
      |}
    """.stripMargin

  @Test
  def `select drop from listSysinfo`: Unit =
    assertEquals(
      """|  public scala.collection.LinearSeq drop(int);
         |  public java.lang.Object drop(int);
         |""".stripMargin,
      Javap.filterSelection("List#drop")(listSysinfo))

  // test -l disassembly
  private val listL =
    """Compiled from "List.scala"
      |public abstract class scala.collection.immutable.List<A> extends scala.collection.immutable.AbstractSeq<A> implements scala.collection.immutable.LinearSeq<A>, scala.collection.StrictOptimizedLinearSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.immutable.StrictOptimizedSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.generic.DefaultSerializable {
      |  public static scala.collection.SeqOps unapplySeq(scala.collection.SeqOps);
      |    LineNumberTable:
      |      line 648: 4
      |
      |  public scala.collection.LinearSeq drop(int);
      |    LineNumberTable:
      |      line 79: 0
      |    LocalVariableTable:
      |      Start  Length  Slot  Name   Signature
      |          0       6     0  this   Lscala/collection/immutable/List;
      |          0       6     1     n   I
      |
      |  public scala.collection.LinearSeq dropWhile(scala.Function1);
      |    LineNumberTable:
      |      line 79: 0
      |    LocalVariableTable:
      |      Start  Length  Slot  Name   Signature
      |          0       6     0  this   Lscala/collection/immutable/List;
      |          0       6     1     p   Lscala/Function1;
      |
      |  public java.lang.Object drop(int);
      |    LineNumberTable:
      |      line 79: 0
      |    LocalVariableTable:
      |      Start  Length  Slot  Name   Signature
      |          0       6     0  this   Lscala/collection/immutable/List;
      |          0       6     1     n   I
      |
      |  public java.lang.Object sorted(scala.math.Ordering);
      |    LineNumberTable:
      |      line 79: 0
      |    LocalVariableTable:
      |      Start  Length  Slot  Name   Signature
      |          0       6     0  this   Lscala/collection/immutable/List;
      |          0       6     1   ord   Lscala/math/Ordering;
      |}
    """.stripMargin

  @Test
  def `select drop from listL`: Unit =
    assertEquals(
      """|  public scala.collection.LinearSeq drop(int);
         |    LineNumberTable:
         |      line 79: 0
         |    LocalVariableTable:
         |      Start  Length  Slot  Name   Signature
         |          0       6     0  this   Lscala/collection/immutable/List;
         |          0       6     1     n   I
         |  public java.lang.Object drop(int);
         |    LineNumberTable:
         |      line 79: 0
         |    LocalVariableTable:
         |      Start  Length  Slot  Name   Signature
         |          0       6     0  this   Lscala/collection/immutable/List;
         |          0       6     1     n   I
         |""".stripMargin,
      Javap.filterSelection("List#drop")(listL))

  // test -v disassembly  // TODO implement
  private val listV =
    """|
    """.stripMargin

  // test -s disassembly
  private val listS =
    """Compiled from "List.scala"
      |public abstract class scala.collection.immutable.List<A> extends scala.collection.immutable.AbstractSeq<A> implements scala.collection.immutable.LinearSeq<A>, scala.collection.StrictOptimizedLinearSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.immutable.StrictOptimizedSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.generic.DefaultSerializable {
      |  public static scala.collection.SeqOps unapplySeq(scala.collection.SeqOps);
      |    descriptor: (Lscala/collection/SeqOps;)Lscala/collection/SeqOps;
      |
      |  public scala.collection.LinearSeq drop(int);
      |    descriptor: (I)Lscala/collection/LinearSeq;
      |
      |  public scala.collection.LinearSeq dropWhile(scala.Function1);
      |    descriptor: (Lscala/Function1;)Lscala/collection/LinearSeq;
      |
      |  public java.lang.Object drop(int);
      |    descriptor: (I)Ljava/lang/Object;
      |
      |  public java.lang.Object sorted(scala.math.Ordering);
      |    descriptor: (Lscala/math/Ordering;)Ljava/lang/Object;
      |}
    """.stripMargin

  @Test
  def `select drop from listS`: Unit =
    assertEquals(
      """|  public scala.collection.LinearSeq drop(int);
         |    descriptor: (I)Lscala/collection/LinearSeq;
         |  public java.lang.Object drop(int);
         |    descriptor: (I)Ljava/lang/Object;
         |""".stripMargin,
      Javap.filterSelection("List#drop")(listS))

  private val listC =
    """Compiled from "List.scala"
      |public abstract class scala.collection.immutable.List<A> extends scala.collection.immutable.AbstractSeq<A> implements scala.collection.immutable.LinearSeq<A>, scala.collection.StrictOptimizedLinearSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.immutable.StrictOptimizedSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.generic.DefaultSerializable {
      |  public static scala.collection.SeqOps unapplySeq(scala.collection.SeqOps);
      |    Code:
      |       0: getstatic     #43                 // Field scala/collection/immutable/List$.MODULE$:Lscala/collection/immutable/List$;
      |       3: pop
      |       4: aload_0
      |       5: areturn
      |
      |  public scala.collection.LinearSeq drop(int);
      |    Code:
      |       0: aload_0
      |       1: iload_1
      |       2: invokestatic  #223                // InterfaceMethod scala/collection/StrictOptimizedLinearSeqOps.drop$:(Lscala/collection/StrictOptimizedLinearSeqOps;I)Lscala/collection/LinearSeq;
      |       5: areturn
      |
      |  public scala.collection.LinearSeq dropWhile(scala.Function1);
      |    Code:
      |       0: aload_0
      |       1: aload_1
      |       2: invokestatic  #230                // InterfaceMethod scala/collection/StrictOptimizedLinearSeqOps.dropWhile$:(Lscala/collection/StrictOptimizedLinearSeqOps;Lscala/Function1;)Lscala/collection/LinearSeq;
      |       5: areturn
      |
      |  public java.lang.Object drop(int);
      |    Code:
      |       0: aload_0
      |       1: iload_1
      |       2: invokevirtual #792                // Method drop:(I)Lscala/collection/LinearSeq;
      |       5: areturn
      |
      |  public java.lang.Object sorted(scala.math.Ordering);
      |    Code:
      |       0: aload_0
      |       1: aload_1
      |       2: invokestatic  #210                // InterfaceMethod scala/collection/immutable/StrictOptimizedSeqOps.sorted$:(Lscala/collection/immutable/StrictOptimizedSeqOps;Lscala/math/Ordering;)Ljava/lang/Object;
      |       5: areturn
      |}
    """.stripMargin

  @Test
  def `select drop from List disassembly`: Unit =
    assertEquals(
      """|  public scala.collection.LinearSeq drop(int);
         |    Code:
         |       0: aload_0
         |       1: iload_1
         |       2: invokestatic  #223                // InterfaceMethod scala/collection/StrictOptimizedLinearSeqOps.drop$:(Lscala/collection/StrictOptimizedLinearSeqOps;I)Lscala/collection/LinearSeq;
         |       5: areturn
         |  public java.lang.Object drop(int);
         |    Code:
         |       0: aload_0
         |       1: iload_1
         |       2: invokevirtual #792                // Method drop:(I)Lscala/collection/LinearSeq;
         |       5: areturn
         |""".stripMargin,
      Javap.filterSelection("List#drop")(listC))

  @Test
  def `select last method from disassembly`: Unit =
    assertEquals(
      """|  public java.lang.Object sorted(scala.math.Ordering);
         |    Code:
         |       0: aload_0
         |       1: aload_1
         |       2: invokestatic  #210                // InterfaceMethod scala/collection/immutable/StrictOptimizedSeqOps.sorted$:(Lscala/collection/immutable/StrictOptimizedSeqOps;Lscala/math/Ordering;)Ljava/lang/Object;
         |       5: areturn
         |""".stripMargin,
      Javap.filterSelection("List#sorted")(listC))
end JavapFilterSelectionTests
