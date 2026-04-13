package dotty.tools.pc.tests.signaturehelp

import scala.meta.pc.SymbolDocumentation

import dotty.tools.pc.base.BaseSignatureHelpSuite
import dotty.tools.pc.utils.MockEntries

import org.junit.Test

class SignatureHelpDocSuite extends BaseSignatureHelpSuite:

  override protected def mockEntries: MockEntries = new MockEntries:
    override def documentations: Set[SymbolDocumentation] = Set(
      MockDocumentation("java/lang/String#substring().", "substring", Seq(), Seq("beginIndex")),
      MockDocumentation("java/lang/String#substring(+1).", "substring", Seq(), Seq("beginIndex", "endIndex")),
      MockDocumentation("java/lang/String#valueOf().", "valueOf", Seq(), Seq("obj")),
      MockDocumentation("java/lang/String#valueOf(+1).", "valueOf", Seq(), Seq("data")),
      MockDocumentation("java/lang/String#valueOf(+2).", "valueOf", Seq(), Seq("data", "offset", "count")),
      MockDocumentation("java/lang/String#valueOf(+3).", "valueOf", Seq(), Seq("b")),
      MockDocumentation("java/lang/String#valueOf(+4).", "valueOf", Seq(), Seq("c")),
      MockDocumentation("java/lang/String#valueOf(+5).", "valueOf", Seq(), Seq("i")),
      MockDocumentation("java/lang/String#valueOf(+6).", "valueOf", Seq(), Seq("l")),
      MockDocumentation("java/lang/String#valueOf(+7).", "valueOf", Seq(), Seq("f")),
      MockDocumentation("java/lang/String#valueOf(+8).", "valueOf", Seq(), Seq("d")),
      MockDocumentation("java/io/File#`<init>`(+2).", "<init>", Seq(), Seq("pathname")),
      MockDocumentation("java/io/File#`<init>`(+3).", "<init>", Seq(), Seq("parent", "child")),
      MockDocumentation("java/io/File#`<init>`(+4).", "<init>", Seq(), Seq("parent", "child")),
      MockDocumentation("java/io/File#`<init>`(+5).", "<init>", Seq(), Seq("uri")),
      ScalaMockDocumentation(
        "java/util/Collections#singleton().",
        "singleton",
        List(MockParam("T")),
        List(MockParam("o"))
      ),
      ScalaMockDocumentation("scala/Some#", "Some"),
      ScalaMockDocumentation(
        "scala/Option#fold().",
        "fold",
        List(MockParam("B")),
        List(MockParam("ifEmpty"), MockParam("f"))
      ),
      ScalaMockDocumentation("scala/Option.apply().", "apply", List(), List(MockParam("x"))),
      ScalaMockDocumentation(
        "scala/collection/immutable/List#map().",
        "map",
        List(MockParam("B")),
        List(MockParam("f"))
      ),
      ScalaMockDocumentation(
        "scala/collection/LinearSeqOps#foldLeft().",
        "foldLeft",
        List(MockParam("B")),
        List(MockParam("z"), MockParam("op"))
      ),
      ScalaMockDocumentation(
        "scala/util/control/Exception.Catch#",
        "Catch",
        List(),
        List(MockParam("pf"), MockParam("fin"), MockParam("rethrow"))
      )
    )

  @Test def `curry` =
    checkDoc(
      """
        |object a {
        |  Option(1).fold("")(_ => @@)
        |}
      """.stripMargin,
      s"""Found documentation for scala/Option#fold().
         |fold[B](ifEmpty: => B)(f: Int => B): B
         |                       ^^^^^^^^^^^
         |  @param B Found documentation for type param B
         |  @param ifEmpty Found documentation for param ifEmpty
         |  @param f Found documentation for param f
          """.stripMargin
    )

  @Test def `curry2` =
    checkDoc(
      """
        |object a {
        |  Option(1).fold("@@")
        |}
      """.stripMargin,
      s"""|Found documentation for scala/Option#fold().
          |fold[B](ifEmpty: => B)(f: Int => B): B
          |        ^^^^^^^^^^^^^
          |  @param B Found documentation for type param B
          |  @param ifEmpty Found documentation for param ifEmpty
          |  @param f Found documentation for param f
          |""".stripMargin
    )

  @Test def `curry3` =
    checkDoc(
      """
        |object a {
        |  List(1).foldLeft(0) {
        |   case @@
        |  }
        |}
      """.stripMargin,
      """|Found documentation for scala/collection/LinearSeqOps#foldLeft().
         |foldLeft[B](z: B)(op: (B, Int) => B): B
         |                  ^^^^^^^^^^^^^^^^^
         |  @param B Found documentation for type param B
         |  @param z Found documentation for param z
         |  @param op Found documentation for param op
         |""".stripMargin
    )

  @Test def `curry4` =
    checkDoc(
      """
        |object a {
        |  def curry(a: Int, b: Int)(c: Int) = a
        |  curry(1)(3@@)
        |}
      """.stripMargin,
      """|
         |curry(a: Int, b: Int)(c: Int): Int
         |                      ^^^^^^
         |""".stripMargin
    )

  @Test def `canbuildfrom` =
    checkDoc(
      """
        |object a {
        |  List(1).map(x => @@)
        |}
      """.stripMargin,
      """|Found documentation for scala/collection/immutable/List#map().
         |map[B](f: Int => B): List[B]
         |       ^^^^^^^^^^^
         |  @param B Found documentation for type param B
         |  @param f Found documentation for param f
         |""".stripMargin
    )

  @Test def `too-many` =
    checkDoc(
      """
        |object a {
        |  Option(1, 2, @@2)
        |}
      """.stripMargin,
      """|Found documentation for scala/Option.apply().
         |apply[A](x: A | Null): Option[A]
         |         ^^^^^^^^^^^
         |  @param x Found documentation for param x
         |""".stripMargin
    )

  @Test def `java5` =
    checkDoc(
      """
        |object a {
        |  java.util.Collections.singleton(@@)
        |}
      """.stripMargin,
      """|Found documentation for java/util/Collections#singleton().
         |singleton[T](o: T): java.util.Set[T]
         |             ^^^^
         |  @param T Found documentation for type param T
         |  @param o Found documentation for param o
         |""".stripMargin
    )

  @Test def `default` =
    checkDoc(
      """
        |object A {
        |  new scala.util.control.Exception.Catch(@@)
        |}
      """.stripMargin,
      """|Found documentation for scala/util/control/Exception.Catch#
         |Catch[T](pf: Catcher[T], fin: Option[Finally], rethrow: Throwable => Boolean)
         |         ^^^^^^^^^^^^^^
         |  @param pf Found documentation for param pf
         |  @param fin Found documentation for param fin
         |  @param rethrow Found documentation for param rethrow
         |""".stripMargin
    )

  @Test def `java` =
    check(
      """
        |object a {
        |  new java.io.File(@@)
        |}
      """.stripMargin,
      """|File(uri: URI)
         |     ^^^^^^^^
         |File(parent: File, child: String)
         |File(parent: String, child: String)
         |File(pathname: String)
         |""".stripMargin
    )

  @Test def `java2` =
    check(
      """
        |object a {
        |  "".substring(1@@)
        |}
      """.stripMargin,
      """|substring(beginIndex: Int, endIndex: Int): String
         |substring(beginIndex: Int): String
         |          ^^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `java3` =
    check(
      """
        |object a {
        |  String.valueOf(1@@)
        |}
      """.stripMargin,
      """|valueOf(d: Double): String
         |valueOf(f: Float): String
         |valueOf(l: Long): String
         |valueOf(i: Int): String
         |        ^^^^^^
         |valueOf(c: Char): String
         |valueOf(b: Boolean): String
         |valueOf(data: Array[Char], offset: Int, count: Int): String
         |valueOf(data: Array[Char]): String
         |valueOf(obj: Object): String
         |""".stripMargin
    )

  @Test def `java4` =
    check(
      """
        |object a {
        |  String.valueOf(@@)
        |}
      """.stripMargin,
      """|valueOf(d: Double): String
         |        ^^^^^^^^^
         |valueOf(f: Float): String
         |valueOf(l: Long): String
         |valueOf(i: Int): String
         |valueOf(c: Char): String
         |valueOf(b: Boolean): String
         |valueOf(data: Array[Char], offset: Int, count: Int): String
         |valueOf(data: Array[Char]): String
         |valueOf(obj: Object): String
         |""".stripMargin
    )

  @Test def `ctor2` =
    checkDoc(
      """
        |object a {
        |  new Some(10@@)
        |}
      """.stripMargin,
      """|Found documentation for scala/Some#
         |Some[A](value: A)
         |        ^^^^^^^^
         |""".stripMargin
    )
