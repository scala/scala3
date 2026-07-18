package dotty.tools.pc.tests.completion

import scala.language.unsafeNulls

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionExtraConstructorSuite extends BaseCompletionSuite:

  @Test def `no-extra-new-completions-class-1` =
    check(
      """|object Wrapper:
         |  class TestClass(x: Int)
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int): TestClass (Constructor)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `no-extra-new-completions-class-2` =
    check(
      """|object Wrapper:
         |  class TestClass()
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Constructor)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `no-extra-new-completions-class-3` =
    check(
      """|object Wrapper:
         |  class TestClass[T](x: T)
         |  TestCla@@
         |""".stripMargin,
      """|TestClass[T](x: T): TestClass[T] (Constructor)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `no-extra-new-completions-case-class-1` =
    check(
      """|object Wrapper:
         |  case class TestClass(x: Int)
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `no-extra-new-completions-case-class-2` =
    check(
      """|object Wrapper:
         |  case class TestClass()
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `no-extra-new-completions-case-class-3` =
    check(
      """|object Wrapper:
         |  case class TestClass[T](x: T)
         |  TestCla@@
         |""".stripMargin,
      """|TestClass[T](x: T): TestClass[T] (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(x: Int): TestClass (Constructor)
  @Test def `extra-new-completions-abstract-class-1` =
    check(
      """|object Wrapper:
         |  abstract class TestClass(x: Int)
         |  TestCla@@
         |""".stripMargin,
      """|
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(): TestClass (Constructor)
  @Test def `extra-new-completions-abstract-class-2` =
    check(
      """|object Wrapper:
         |  abstract class TestClass()
         |  TestCla@@
         |""".stripMargin,
      """|
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass[T](x: T): TestClass[T] (Constructor)
  @Test def `extra-new-completions-abstract-class-3` =
    check(
      """|object Wrapper:
         |  abstract class TestClass[T](x: T)
         |  TestCla@@
         |""".stripMargin,
      """|
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass (Constructor)
  @Test def `extra-new-completions-trait-1` =
    check(
      """|object Wrapper:
         |  trait TestClass
         |  TestCla@@
         |""".stripMargin,
      """|
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `extra-new-completions-class-1` =
    check(
      """|object Wrapper:
         |  class TestClass(x: Int)
         |  object TestClass:
         |    def apply(x: Int, y: Int): TestClass = TestClass(x + y)
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int, y: Int): TestClass (Method)
         |new TestClass(x: Int): TestClass (Constructor)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `extra-new-completions-class-2` =
    check(
      """|object Wrapper:
         |  class TestClass(x: Int)
         |  object TestClass:
         |    def apply(x: Int): TestClass = TestClass(x)
         |  TestCla@@
         |}
         |""".stripMargin,
      """|TestClass(x: Int): TestClass (Method)
         |new TestClass(x: Int): TestClass (Constructor)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `extra-new-completions-class-3` =
    check(
      """|object Wrapper:
         |  class TestClass()
         |  object TestClass:
         |    def apply(): TestClass = TestClass(1)
         |    TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Method)
         |new TestClass(): TestClass (Constructor)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(x: Int): TestClass (Constructor)
  @Test def `extra-new-completions-abstract-class-with-companion-1` =
    check(
      """|object Wrapper:
         |  abstract class TestClass(x: Int)
         |  object TestClass:
         |    def apply(x: Int, y: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int, y: Int): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(x: Int): TestClass (Constructor)
  @Test def `extra-new-completions-abstract-class-with-companion-2` =
    check(
      """|object Wrapper:
         |  abstract class TestClass(x: Int)
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(): TestClass (Constructor)
  @Test def `extra-new-completions-abstract-class-with-companion-3` =
    check(
      """|object Wrapper:
         |  abstract class TestClass()
         |  object TestClass:
         |    def apply(): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(x: Int): TestClass (Constructor)
  @Test def `extra-new-completions-trait-with-companion-1` =
    check(
      """|object Wrapper:
         |  trait TestClass(x: Int)
         |  object TestClass:
         |    def apply(x: Int, y: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int, y: Int): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(x: Int): TestClass (Constructor)
  @Test def `extra-new-completions-trait-with-companion-2` =
    check(
      """|object Wrapper:
         |  trait TestClass(x: Int)
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(): TestClass (Constructor)
  @Test def `extra-new-completions-trait-with-companion-3` =
    check(
      """|object Wrapper:
         |  trait TestClass()
         |  object TestClass:
         |    def apply(): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // This test should have new TestClass completion without parentheses. The actual issue is with printer, edit text is correct
  // TODO We first need to detect support when to add additional braces / colon
  // missing new TestClass(): TestClass (Constructor)
  @Test def `extra-new-completions-trait-with-companion-4` =
    check(
      """|object Wrapper:
         |  trait TestClass
         |  object TestClass:
         |    def apply(): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )
    checkSnippet(
      """|object Wrapper:
         |  trait TestClass
         |  object TestClass:
         |    def apply(): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass()
         |TestClass
         |""".stripMargin
    )

  @Test def `multiple-extra-new-constructors-class-1` =
    check(
      """|object Wrapper:
         |  class TestClass():
         |    def this(x: Int) = this()
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Constructor)
         |TestClass(x: Int): TestClass (Constructor)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `multiple-extra-new-constructors-class-2` =
    check(
      """|object Wrapper:
         |  class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Constructor)
         |TestClass(x: Int): TestClass (Constructor)
         |TestClass(x: Int, y: Int): TestClass (Constructor)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `multiple-extra-new-constructors-with-companion-2` =
    check(
      """|object Wrapper:
         |  class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  object TestClass:
         |    def apply(z: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(z: Int): TestClass (Method)
         |new TestClass(): TestClass (Constructor)
         |new TestClass(x: Int): TestClass (Constructor)
         |new TestClass(x: Int, y: Int): TestClass (Constructor)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `multiple-extra-new-constructors-with-companion-3` =
    check(
      """|object Wrapper:
         |  class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  object TestClass:
         |    def apply(z: Int): TestClass = ???
         |    def apply(z: Int, w: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(z: Int): TestClass (Method)
         |TestClass(z: Int, w: Int): TestClass (Method)
         |new TestClass(): TestClass (Constructor)
         |new TestClass(x: Int): TestClass (Constructor)
         |new TestClass(x: Int, y: Int): TestClass (Constructor)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `multiple-extra-new-constructors-with-companion-same-signature-class` =
    check(
      """|object Wrapper:
         |  class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int): TestClass (Method)
         |new TestClass(): TestClass (Constructor)
         |new TestClass(x: Int): TestClass (Constructor)
         |new TestClass(x: Int, y: Int): TestClass (Constructor)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `multiple-extra-new-constructors-with-companion-same-signature-case-class` =
    check(
      """|object Wrapper:
         |  case class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(): TestClass (Method)
         |TestClass(x: Int): TestClass (Method)
         |new TestClass(x: Int): TestClass (Constructor)
         |new TestClass(x: Int, y: Int): TestClass (Constructor)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `multiple-extra-new-constructors-with-companion-same-signature-trait` =
    check(
      """|object Wrapper:
         |  trait TestClass
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  // TODO We first need to detect support when to add additional braces / colon
  // missing:
  // new TestClass(): TestClass (Constructor)
  // new TestClass(x: Int): TestClass (Constructor)
  // new TestClass(x: Int, y: Int): TestClass (Constructor)
  @Test def `multiple-extra-new-constructors-with-companion-same-signature-abstract` =
    check(
      """|object Wrapper:
         |  abstract class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |  TestCla@@
         |""".stripMargin,
      """|TestClass(x: Int): TestClass (Method)
         |TestClass test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `no-extra-completions-in-type-mode-1` =
    check(
      """|object Wrapper:
         |  class TestClass()
         |  val x: TestCla@@
         |""".stripMargin,
      """|TestClass test.Wrapper (Class)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `no-extra-completions-in-type-mode-2` =
    check(
      """|object Wrapper:
         |  class TestClass()
         |  val x: TestCla@@
         |""".stripMargin,
      """|TestClass test.Wrapper (Class)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `no-extra-completions-in-type-mode-3` =
    check(
      """|object Wrapper:
         |  class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |  val x: TestCla@@
         |""".stripMargin,
      """|TestClass test.Wrapper (Class)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `workspace-no-extra-completions-in-type-mode-4` =
    check(
      """|object Wrapper:
         |  class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |object M {
         |  val x: TestCla@@
         |}
         |""".stripMargin,
      """|TestClass - test.Wrapper (Class)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `workspace-multiple-extra-new-constructors` =
    check(
      """|object Wrapper:
         |  class TestClass():
         |    def this(x: Int) = this()
         |    def this(x: Int, y: Int) = this()
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |object M {
         |  TestCla@@
         |}
         |""".stripMargin,
      """|TestClass(x: Int): TestClass - test.Wrapper (Method)
         |new TestClass(): TestClass - test.Wrapper (Constructor)
         |new TestClass(x: Int): TestClass - test.Wrapper (Constructor)
         |new TestClass(x: Int, y: Int): TestClass - test.Wrapper (Constructor)
         |TestClass - test.Wrapper (Module)
         |""".stripMargin,
      includeCompletionKind = true
    )

  @Test def `prepend-new` =
    checkSnippet(
      """|object Wrapper:
         |  case class TestClass(x: Int)
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |object Main {
         |  TestClas@@
         |}
         |""".stripMargin,
      """|TestClass($0)
         |new TestClass
         |TestClass
         |""".stripMargin
    )

  @Test def `prepend-new-fully-qualified-path` =
    checkSnippet(
      """|object Wrapper:
         |  case class TestClass(x: Int)
         |  object TestClass:
         |    def apply(x: Int): TestClass = ???
         |object Main {
         |  Wrapper.Test@@
         |}
         |""".stripMargin,
      """|TestClass($0)
         |new Wrapper.TestClass
         |TestClass
         |""".stripMargin
    )

  @Test def `dont-include-private-members` =
    check(
      """|object TestObject:
         |  private def apply(i: Int) = i
         |object Main:
         |  TestObject@@
         |""".stripMargin,
      """|TestObject test
         |""".stripMargin
    )
