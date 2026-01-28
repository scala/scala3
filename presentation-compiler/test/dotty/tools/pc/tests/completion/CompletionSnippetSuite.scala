package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Ignore
import org.junit.Test

class CompletionSnippetSuite extends BaseCompletionSuite:

  @Test def `member` =
    checkSnippet(
      """
        |object Main {
        |  List.appl@@
        |}
        |""".stripMargin,
      """|apply($0)
         |unapplySeq($0)
         |""".stripMargin
    )

  @Test def `scope` =
    checkSnippet(
      """
        |object Main {
        |  printl@@
        |
        |}
        |""".stripMargin,
      """|println()
         |println($0)
         |""".stripMargin
    )

  @Test def `nullary` =
    checkSnippet(
      """
        |object Main {
        |  List(1).hea@@
        |}
        |""".stripMargin,
      """|head
         |headOption
         |""".stripMargin
    )

  @Test def `nilary` =
    checkSnippet(
      s"""|class Hello{
          |  def now() = 25
          |}
          |object Main {
          |  val h = new Hello()
          |  h.no@@
          |}
          |""".stripMargin,
      """|now()
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `java-nullary` =
    checkSnippet(
      """
        |class Foo {
        |  override def toString = "Foo"
        |}
        |object Main {
        |  new Foo().toStrin@@
        |
        |}
        |""".stripMargin,
      // even if `Foo.toString` is nullary, it overrides `Object.toString()`
      // which is a Java non-nullary method with an empty parameter list.
      """|toString
         |""".stripMargin
    )

  // Dotty does not currently support fuzzy completions. Please take a look at
  // https://github.com/lampepfl/dotty-feature-requests/issues/314
  @Ignore("Fuzzy should be provided by dotty")
  @Test def `type-empty` =
    if scala.util.Properties.isJavaAtLeast("9") then
      checkSnippet(
        """
          |object Main {
          |  type MyType = List[Int]
          |  def list : MT@@
          |}
          |""".stripMargin,
        """|MyType
           |""".stripMargin
      )
    else
      checkSnippet(
        """
          |object Main {
          |  type MyType = List[Int]
          |  def list : MT@@
          |}
          |""".stripMargin,
        """|MyType
           |MTOM
           |MTOMFeature
           |""".stripMargin
      )

    // Dotty does not currently support fuzzy completions. Please take a look at
    // https://github.com/lampepfl/dotty-feature-requests/issues/314
  @Ignore("Fuzzy should be provided by dotty")
  @Test def `type-new-empty` =
    if scala.util.Properties.isJavaAtLeast("9") then
      checkSnippet(
        """
          |object Main {
          |  class Gen[T]
          |  type MyType = Gen[Int]
          |  new MT@@
          |}
          |""".stripMargin,
        """|MyType
           |""".stripMargin
      )
    else
      checkSnippet(
        """
          |object Main {
          |  class Gen[T]
          |  type MyType = Gen[Int]
          |  new MT@@
          |}
          |""".stripMargin,
        """|MyType
           |MTOM
           |MTOMFeature
           |""".stripMargin
      )

  @Test def `type` =
    checkSnippet(
      s"""|object Main {
          |  val x: scala.IndexedSe@@
          |}
          |""".stripMargin,
      // It's expected to have two separate results, one for `object IndexedSeq` (which should not
      // expand snipppet) and one for `type IndexedSeq[T]`.
      """|IndexedSeq[$0]
         |IndexedSeq
         |""".stripMargin
    )

  @Test def `empty-params-with-implicit` =
    checkSnippet(
      s"""|object Main {
          |  def doSomething()(implicit x: Int) = x
          |  val bar = doSomethi@@
          |}
          |""".stripMargin,
      "doSomething($0)"
    )

  @Test def `type3` =
    checkSnippet(
      s"""|object Main {
          |  def foo(param: ArrayDeque@@)
          |}
          |""".stripMargin,
      // ArrayDeque upper is for java, the lower for scala
      """|ArrayDeque[$0]
         |ArrayDeque[$0]
         |ArrayDequeOps[$0]
         |ArrayDeque
         |ArrayDeque
         |""".stripMargin
    )

  @Test def `type4` =
    checkSnippet(
      s"""|object Main {
          |  new SimpleFileVisitor@@
          |}
          |""".stripMargin,
      """|SimpleFileVisitor[$0]
         |SimpleFileVisitor
         |""".stripMargin
    )

  @Test def `type5` =
    checkSnippet(
      s"""|object Main {
          |  new scala.Iterabl@@
          |}
          |""".stripMargin,
      """|Iterable[$0] {}
         |IterableOnce[$0] {}
         |Iterable
         |""".stripMargin
    )

  @Test def `type6` =
    checkSnippet(
      s"""|object Main {
          |  def foo: scala.Iterable@@
          |}
          |""".stripMargin,
      """|Iterable[$0]
         |IterableOnce[$0]
         |Iterable
         |""".stripMargin
    )

  @Test def `type7` =
    checkSnippet(
      s"""|object Main {
          |  def foo(param: List[scala.Iterable@@])
          |}
          |""".stripMargin,
      """|Iterable[$0]
         |IterableOnce[$0]
         |Iterable
         |""".stripMargin
    )

  @Test def `type8` =
    checkSnippet(
      s"""|
          |class Base {
          |  class Inner
          |}
          |object Upper extends Base
          |object Main {
          |  def foo(param: Uppe@@)
          |}
          |""".stripMargin,
      """|Upper
         |""".stripMargin
    )

  @Test def `trailing-paren` =
    checkEditLine(
      s"""|object Main {
          |  def trailing(a: Int) = ()
          |  ___
          |}
          |""".stripMargin,
      "trailing@@()",
      "trailing()"
    )

  @Test def `trailing-brace` =
    checkEditLine(
      s"""|object Main {
          |  def trailing(a: Int) = ()
          |  ___
          |}
          |""".stripMargin,
      "trailing@@ { }",
      "trailing { }"
    )

  @Test def `trailing-brace1` =
    checkEditLine(
      s"""|object Main {
          |  def trailing(a: Int) = ()
          |  ___
          |}
          |""".stripMargin,
      "trailing@@{ }",
      "trailing{ }"
    )

  @Test def `implicit` =
    checkEditLine(
      s"""|object Main {
          |  ___
          |}
          |""".stripMargin,
      "List(1).flatte@@",
      "List(1).flatten"
    )

  // no completions are suggested if we already have full typ =
  @Test def `bug1` =
    checkEditLine(
      s"""|object Main {
          |  ___
          |}
          |""".stripMargin,
      "scala.util.Try@@(1)",
      "scala.util.Try(1)",
      assertSingleItem = false
    )

  @Test def `case-class` =
    checkEditLine(
      s"""|object Main {
          |  ___
          |}
          |""".stripMargin,
      "scala.util.Tr@@(1)",
      "scala.util.Try(1)",
      filter = str => str.contains("Try"),
      assertSingleItem = false
    )

  @Test def `case-class2` =
    checkSnippet(
      s"""|object wrapper:
          |  case class Test2(x: Int)
          |  object Test2:
          |    def apply(x: Int): Test2 = ???
          |object Main {
          |  wrapper.Test@@
          |}
          |""".stripMargin,
      """|Test2($0)
         |new wrapper.Test2
         |Test2
         |""".stripMargin
    )

  @Test def `case-class2-edit` =
    checkEditLine(
      s"""|object wrapper:
          |  case class Test2(x: Int)
          |  object Test2:
          |    def apply(x: Int): Test2 = ???
          |object Main {
          |  ___
          |}
          |""".stripMargin,
      "wrapper.Test@@",
      "new wrapper.Test2",
      filter = _.contains("new Test2")
    )

  @Test def `case-class3` =
    checkSnippet(
      s"""|object Main {
          |  Try@@
          |}
          |""".stripMargin,
      // Note: the class and trait items in here are invalid. So
      // they are filtered out.
      """|Try($0) - [T](r: => T): Try[T]
         |Try -  scala.util
         |""".stripMargin,
      includeDetail = true
    )

  @Test def `symbol` =
    checkEditLine(
      s"""|object Main {
          |  val out = new StringBuilder()
          |  ___
          |}
          |""".stripMargin,
      "out.+@@=('a')",
      "out.++==('a')",
      filter = _.contains("++=(s: String)")
    )

  @Test def `multiple-apply` =
    checkSnippet(
      s"""|package example
          |
          |case class Widget(name: String, age: Int)
          |object Widget{
          |  def apply(name: String): Widget = Widget(name, 0)
          |  def apply(age: Int): Widget = Widget("name", age)
          |}
          |object Main {
          |  Wi@@
          |}
          |""".stripMargin,
      """|Widget($0) - (name: String): Widget
         |Widget($0) - (age: Int): Widget
         |Widget($0) - (name: String, age: Int): Widget
         |Widget -  example
         |""".stripMargin,
      includeDetail = true,
      topLines = Some(4)
    )

  @Ignore
  @Test def `no-apply` =
    checkSnippet(
      s"""|package example
          |
          |object Widget{}
          |object Main {
          |  Wi@@
          |}
          |""".stripMargin,
      """|Widget -  example
         |Window -  java.awt
         |WindowPeer -  java.awt.peer
         |WithFilter - [A](p: A => Boolean, xs: Array[A]): WithFilter[A]
         |WithFilter - [A, CC[_$$2]](self: IterableOps[A, CC, ?], p: A => Boolean): WithFilter[A, CC]
         |WithFilter - [K, V, IterableCC[_$$3], CC[_$$4,_$$5] <: IterableOps[?, AnyConstr, ?]](self: MapOps[K, V, CC, ?] & IterableOps[(K, V), IterableCC, ?], p: ((K, V)) => Boolean): WithFilter[K, V, IterableCC, CC]
         |WithFilter - [K, V, IterableCC[_$$1], MapCC[X,Y] <: scala.collection.Map[X, Y], CC[X,Y] <: scala.collection.Map[X, Y] & SortedMapOps[X, Y, CC, ?]](self: SortedMapOps[K, V, CC, ?] & MapOps[K, V, MapCC, ?] & IterableOps[(K, V), IterableCC, ?], p: ((K, V)) => Boolean): WithFilter[K, V, IterableCC, MapCC, CC]
         |WithFilter - [A, IterableCC[_$$1], CC[X] <: SortedSet[X]](self: SortedSetOps[A, CC, ?] & IterableOps[A, IterableCC, ?], p: A => Boolean): WithFilter[A, IterableCC, CC]
         |WithFilter - (p: Char => Boolean, s: String): WithFilter
         |WithFilter - [A](l: Stream[A] @uncheckedVariance, p: A => Boolean): WithFilter[A]
         |""".stripMargin,
      includeDetail = true
    )

  @Test def `no-apply2` =
    checkSnippet(
      s"""|package example
          |
          |object TestObject {}
          |object Main {
          |  TestObjec@@
          |}
          |""".stripMargin,
      """|TestObject -  example
         |""".stripMargin,
      includeDetail = true
    )

  @Test def `dont-enter-empty-paramlist` =
    checkSnippet(
      s"""|package example
          |
          |object Main {
          |  ListMa@@
          |}
          |""".stripMargin,
      """|ListMap($0) - [K, V](elems: (K, V)*): ListMap[K, V]
         |new ListMap - [K, V]: ListMap[K, V]
         |ListMap -  scala.collection.immutable
         |ListMap($0) - [K, V](elems: (K, V)*): ListMap[K, V]
         |new ListMap - [K, V]: ListMap[K, V]
         |ListMap -  scala.collection.mutable
         |ListMapBuilder - [K, V]: ListMapBuilder[K, V]
         |ConcurrentSkipListMap -  java.util.concurrent
         |""".stripMargin,
      includeDetail = true
    )

  // https://github.com/scalameta/metals/issues/4004
  @Test def `extension-param1` =
    checkEdit(
      s"""|package a
          |object Foo:
          |  extension (s: String)
          |    def bar = 0
          |  val bar = "abc".ba@@
      """.stripMargin,
      s"""|package a
          |object Foo:
          |  extension (s: String)
          |    def bar = 0
          |  val bar = "abc".bar
      """.stripMargin,
      filter = _.contains("bar: Int")
    )

  // https://github.com/scalameta/metals/issues/4004
  @Test def `extension-param2` =
    checkEdit(
      s"""|package a
          |object Foo:
          |  extension (s: String)
          |    def bar() = 0
          |  val bar = "abc".ba@@
      """.stripMargin,
      s"""|package a
          |object Foo:
          |  extension (s: String)
          |    def bar() = 0
          |  val bar = "abc".bar()
      """.stripMargin,
      filter = _.contains("bar: Int")
    )

  @Test def `brackets-already-present` =
    check(
      """|package a
         |case class AAA[T]()
         |object O {
         |  val l: AA@@[Int] = ???
         |}
         |""".stripMargin,
      """|AAA a
         |ArrowAssoc scala.Predef
         |""".stripMargin
    )

  @Test def `brackets-already-present-edit` =
    checkEdit(
      """|package a
         |case class AAA[T]()
         |object O {
         |  val l: AA@@[Int] = ???
         |}
         |""".stripMargin,
      """|package a
         |case class AAA[T]()
         |object O {
         |  val l: AAA[Int] = ???
         |}
         |""".stripMargin,
      assertSingleItem = false
    )
