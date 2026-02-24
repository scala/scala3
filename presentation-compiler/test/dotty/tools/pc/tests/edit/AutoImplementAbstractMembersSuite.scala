package dotty.tools.pc.tests.edit

import java.net.URI

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams

import dotty.tools.pc.base.BaseCodeActionSuite
import dotty.tools.pc.utils.TextEdits

import org.eclipse.lsp4j as l
import org.junit.Test

class AutoImplementAbstractMembersSuite extends BaseCodeActionSuite:

  @Test def `classdef` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class <<Concrete>> extends Base {
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class Concrete extends Base {
         |
         |    override def foo(x: Int): Int = ???
         |
         |    override def bar(x: String): String = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `classdef-tparam` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class <<Concrete>>[T] extends Base
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class Concrete[T] extends Base {
         |
         |    override def foo(x: Int): Int = ???
         |
         |    override def bar(x: String): String = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `no-new-line` =
    checkEdit(
      """|package a
          |
          |trait X:
          |  def foo: Unit
          |
          |class <<Y>> extends X""".stripMargin,
      """|package a
         |
         |trait X:
         |  def foo: Unit
         |
         |class Y extends X {
         |
         |  override def foo: Unit = ???
         |
         |}""".stripMargin
    )

  @Test def `empty-lines-between-members` =
    checkEdit(
      """|package a
        |
        |object A {
        |  trait Base {
        |    def foo(x: Int): Int
        |    def bar(x: String): String
        |  }
        |  class <<Concrete>> extends Base {
        |
        |    def bar(x: String): String = ???
        |
        |  }
        |}
        |""".stripMargin,
      """|package a
        |
        |object A {
        |  trait Base {
        |    def foo(x: Int): Int
        |    def bar(x: String): String
        |  }
        |  class Concrete extends Base {
        |
        |
        |    override def foo(x: Int): Int = ???
        |
        |    def bar(x: String): String = ???
        |
        |  }
        |}
        |""".stripMargin
    )

  @Test def `objectdef` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |  }
         |  object <<Concrete>> extends Base {
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |  }
         |  object Concrete extends Base {
         |
         |    override def foo(x: Int): Int = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `overload` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class <<Concrete>> extends Base {
         |    override def foo(x: Int): Int = x
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class Concrete extends Base {
         |
         |    override def bar(x: String): String = ???
         |
         |    override def foo(x: Int): Int = x
         |  }
         |}
         |""".stripMargin
    )

  @Test def `braces` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class <<Concrete>> extends Base {}
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class Concrete extends Base {
         |
         |    override def foo(x: Int): Int = ???
         |
         |    override def bar(x: String): String = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `object-creation` =
    checkEdit(
      """
        |object Main {
        |  new <<Iterable>>[Int] {}
        |}
      """.stripMargin,
      """
        |object Main {
        |  new Iterable[Int] {
        |
        |    override def iterator: Iterator[Int] = ???
        |
        |  }
        |}
        |""".stripMargin
    )

  @Test def `context-bound` =
    checkEdit(
      """
        |trait Context {
        |   def add[T:Ordering]: T
        |}
        |object Main {
        |  new <<Context>> {
        |  }
        |}
       """.stripMargin,
      """
        |trait Context {
        |   def add[T:Ordering]: T
        |}
        |object Main {
        |  new Context {
        |
        |    override def add[T: Ordering]: T = ???
        |
        |  }
        |}
        |""".stripMargin
    )

  @Test def `generics-inheritance` =
    checkEdit(
      """
        |trait Context[T] {
        |   def method: T
        |}
        |object Main {
        |  class <<Concrete>> extends Context[Int] {
        |  }
        |}
       """.stripMargin,
      """
        |trait Context[T] {
        |   def method: T
        |}
        |object Main {
        |  class Concrete extends Context[Int] {
        |
        |    override def method: Int = ???
        |
        |  }
        |}
        |""".stripMargin
    )

  @Test def `ignore-non-abstract` =
    checkEdit(
      """
        |trait Abstract {
        |  def aaa: Int
        |  def bbb: Int = 2 // should be ignored
        |  type TypeAlias = String // should be ignored
        |}
        |object Main {
        |  new <<Abstract>> {
        |  }
        |}
       """.stripMargin,
      """
        |trait Abstract {
        |  def aaa: Int
        |  def bbb: Int = 2 // should be ignored
        |  type TypeAlias = String // should be ignored
        |}
        |object Main {
        |  new Abstract {
        |
        |    override def aaa: Int = ???
        |
        |  }
        |}
        |""".stripMargin
    )

  @Test def `import` =
    checkEdit(
      """|abstract class Mutable {
         |  def foo: scala.collection.mutable.Set[Int]
         |  def bar: scala.collection.immutable.Set[Int]
         |}
         |object Main {
         |  new <<Mutable>> {
         |  }
         |}
         |""".stripMargin,
      """|import scala.collection.mutable
         |abstract class Mutable {
         |  def foo: scala.collection.mutable.Set[Int]
         |  def bar: scala.collection.immutable.Set[Int]
         |}
         |object Main {
         |  new Mutable {
         |
         |    override def foo: mutable.Set[Int] = ???
         |
         |    override def bar: Set[Int] = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `nested-inheritance` =
    checkEdit(
      """|abstract class SuperAbstract {
         |  def foo: Int
         |}
         |trait Bar extends SuperAbstract {
         |  def bar: Int
         |}
         |object Main {
         |  class <<Baz>> extends Bar {
         |  }
         |}
         |""".stripMargin,
      """|abstract class SuperAbstract {
         |  def foo: Int
         |}
         |trait Bar extends SuperAbstract {
         |  def bar: Int
         |}
         |object Main {
         |  class Baz extends Bar {
         |
         |    override def bar: Int = ???
         |
         |    override def foo: Int = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `jutil` =
    checkEdit(
      """|abstract class JUtil {
         |  def foo: java.util.List[Int]
         |}
         |class <<Main>> extends JUtil {
         |}
         |""".stripMargin,
      """|import java.{util => ju}
         |abstract class JUtil {
         |  def foo: java.util.List[Int]
         |}
         |class Main extends JUtil {
         |
         |  override def foo: ju.List[Int] = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `jutil-multiple-symbols` =
    checkEdit(
      """|abstract class JUtil {
         |  def foo(x: java.util.List[Int]): java.util.List[Int]
         |}
         |class <<Main>> extends JUtil {
         |}
         |""".stripMargin,
      """|import java.{util => ju}
         |abstract class JUtil {
         |  def foo(x: java.util.List[Int]): java.util.List[Int]
         |}
         |class Main extends JUtil {
         |
         |  override def foo(x: ju.List[Int]): ju.List[Int] = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `jutil-conflict` =
    checkEdit(
      """|package jutil
         |abstract class JUtil {
         |  def foo: java.util.List[Int]
         |}
         |class <<Main>> extends JUtil {
         |  val java = 42
         |}
         |""".stripMargin,
      // Ensure we don't insert `_root_` prefix for import because `val java = 42` is local.
      """|package jutil
         |
         |import java.{util => ju}
         |abstract class JUtil {
         |  def foo: java.util.List[Int]
         |}
         |class Main extends JUtil {
         |
         |  override def foo: ju.List[Int] = ???
         |
         |  val java = 42
         |}
         |""".stripMargin
    )

  @Test def `val` =
    checkEdit(
      """|abstract class Abstract {
         |  val baz: String
         |}
         |class <<Main>> extends Abstract {
         |}
         |""".stripMargin,
      """|abstract class Abstract {
         |  val baz: String
         |}
         |class Main extends Abstract {
         |
         |  override val baz: String = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `indent-def` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  // if there's a member, infer indent based on it.
         |  class <<Concrete>> extends Base {
         |       override def foo(x: Int): Int = x
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  // if there's a member, infer indent based on it.
         |  class Concrete extends Base {
         |
         |       override def bar(x: String): String = ???
         |
         |       override def foo(x: Int): Int = x
         |  }
         |}
         |""".stripMargin
    )

  @Test def `indent-val` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  // if there's a member, infer indent based on it.
         |  class <<Concrete>> extends Base {
         |           val test = 1
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  // if there's a member, infer indent based on it.
         |  class Concrete extends Base {
         |
         |           override def foo(x: Int): Int = ???
         |
         |           override def bar(x: String): String = ???
         |
         |           val test = 1
         |  }
         |}
         |""".stripMargin
    )

  @Test def `indent-type` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  // if there's a member, infer indent based on it.
         |  class <<Concrete>> extends Base {
         |           type T = Int
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  // if there's a member, infer indent based on it.
         |  class Concrete extends Base {
         |
         |           override def foo(x: Int): Int = ???
         |
         |           override def bar(x: String): String = ???
         |
         |           type T = Int
         |  }
         |}
         |""".stripMargin
    )

  @Test def `indent-object-creation` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  new <<Base>> {
         |          def bar(x: String): Int = x
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  new Base {
         |
         |          override def foo(x: Int): Int = ???
         |
         |          def bar(x: String): Int = x
         |  }
         |}
         |""".stripMargin
    )

  @Test def `infer-indent-constructor` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  <<class>> Concrete(num: Int) extends Base {
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class Concrete(num: Int) extends Base {
         |
         |    override def foo(x: Int): Int = ???
         |
         |    override def bar(x: String): String = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `infer-indent-auxiliary-constructor` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  <<class>> Concrete(num: Int) extends Base {
         |      def this() = { this(4) }
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class Concrete(num: Int) extends Base {
         |
         |      override def foo(x: Int): Int = ???
         |
         |      override def bar(x: String): String = ???
         |
         |      def this() = { this(4) }
         |  }
         |}
         |""".stripMargin
    )

  @Test def `indent-closing-brace` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |  }
         |      new <<Base>> {}
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |  }
         |      new Base {
         |
         |        override def foo(x: Int): Int = ???
         |
         |      }
         |}
         |""".stripMargin
    )

  @Test def `complete-braces-moduledef` =
    checkEdit(
      """|package a
         |
         |trait Base {
         |  def foo(x: Int): Int
         |}
         |object <<Concrete>> extends Base
         |""".stripMargin,
      """|package a
         |
         |trait Base {
         |  def foo(x: Int): Int
         |}
         |object Concrete extends Base {
         |
         |  override def foo(x: Int): Int = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `access-modifiers` =
    checkEdit(
      """|package a
         |
         |trait Base {
         |  // private is not available for abstract members
         |  protected[a] def f(): Unit
         |  protected def d(): Unit
         |  protected[a] val s: Unit
         |  implicit val a: String
         |  // lazy values might not be abstract
         |}
         |object Test {
         |   class <<Concrete>> extends Base
         |}
         |""".stripMargin,
      """|package a
         |
         |trait Base {
         |  // private is not available for abstract members
         |  protected[a] def f(): Unit
         |  protected def d(): Unit
         |  protected[a] val s: Unit
         |  implicit val a: String
         |  // lazy values might not be abstract
         |}
         |object Test {
         |   class Concrete extends Base {
         |
         |     override protected[a] def f(): Unit = ???
         |
         |     override protected def d(): Unit = ???
         |
         |     override protected[a] val s: Unit = ???
         |
         |     override implicit val a: String = ???
         |
         |   }
         |}
         |""".stripMargin
    )

  @Test def `complete-braces-indent` =
    checkEdit(
      """|package a
         |
         |trait Base {
         |  def foo(x: Int): Int
         |}
         |object Test {
         |   class <<Concrete>> extends Base
         |}
         |""".stripMargin,
      """|package a
         |
         |trait Base {
         |  def foo(x: Int): Int
         |}
         |object Test {
         |   class Concrete extends Base {
         |
         |     override def foo(x: Int): Int = ???
         |
         |   }
         |}
         |""".stripMargin
    )

  @Test def `selftype` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class <<Concrete>> extends Base { self =>
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class Concrete extends Base { self =>
         |
         |    override def foo(x: Int): Int = ???
         |
         |    override def bar(x: String): String = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `selftype-arrow` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class <<Concrete>> extends Base { // > reference
         |    self =>
         |  }
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base {
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |  }
         |  class Concrete extends Base { // > reference
         |    self =>
         |
         |    override def foo(x: Int): Int = ???
         |
         |    override def bar(x: String): String = ???
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `tab-indented1` =
    checkEdit(
      """|package a
         |
         |object A {
         |	trait Base {
         |		def foo(x: Int): Int
         |		def bar(x: String): String
         |	}
         |	class <<Concrete>> extends Base {
         |	}
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |	trait Base {
         |		def foo(x: Int): Int
         |		def bar(x: String): String
         |	}
         |	class Concrete extends Base {
         |
         |		override def foo(x: Int): Int = ???
         |
         |		override def bar(x: String): String = ???
         |
         |	}
         |}
         |""".stripMargin
    )

  @Test def `tab-indented2` =
    checkEdit(
      """|package a
         |
         |object A {
         |	trait Base {
         |		def foo(x: Int): Int
         |		def bar(x: String): String
         |	}
         |	class <<Concrete>> extends Base
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |	trait Base {
         |		def foo(x: Int): Int
         |		def bar(x: String): String
         |	}
         |	class Concrete extends Base {
         |
         |		override def foo(x: Int): Int = ???
         |
         |		override def bar(x: String): String = ???
         |
         |	}
         |}
         |""".stripMargin
    )

  @Test def `braceless-basic` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base:
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |
         |  class <<Concrete>> extends Base:
         |    def foo(x: Int): Int = x
         |
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base:
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |
         |  class Concrete extends Base:
         |
         |    override def bar(x: String): String = ???
         |
         |    def foo(x: Int): Int = x
         |
         |}
         |""".stripMargin
    )

  @Test def `braceless-selftype` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base:
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |
         |  class <<Concrete>> extends Base:
         |    def foo(x: Int): Int = x
         |
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base:
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |
         |  class Concrete extends Base:
         |
         |    override def bar(x: String): String = ???
         |
         |    def foo(x: Int): Int = x
         |
         |}
         |""".stripMargin
    )

  @Test def `tab-indented-braceless` =
    checkEdit(
      """|package a
         |
         |trait Base:
         |	def foo(x: Int): Int
         |	def bar(x: String): String
         |
         |class <<Concrete>> extends Base:
         |	def foo(x: Int): Int = x
         |""".stripMargin,
      """|package a
         |
         |trait Base:
         |	def foo(x: Int): Int
         |	def bar(x: String): String
         |
         |class Concrete extends Base:
         |
         |	override def bar(x: String): String = ???
         |
         |	def foo(x: Int): Int = x
         |
         |""".stripMargin
    )

  @Test def `extension-methods` =
    checkEdit(
      """|package a
         |
         |trait Base:
         |  extension (x: Int)
         |    def foo: Int
         |    def bar: String
         |
         |class <<Concrete>> extends Base
         |""".stripMargin,
      """|package a
         |
         |trait Base:
         |  extension (x: Int)
         |    def foo: Int
         |    def bar: String
         |
         |class Concrete extends Base {
         |
         |  extension (x: Int) override def foo: Int = ???
         |
         |  extension (x: Int) override def bar: String = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `extension-methods-tparam` =
    checkEdit(
      """|package a
         |
         |trait Base[T]:
         |  extension (x: T)
         |    def foo: Int
         |    def bar: String
         |
         |class <<Concrete>>[T] extends Base[Int]
         |""".stripMargin,
      """|package a
         |
         |trait Base[T]:
         |  extension (x: T)
         |    def foo: Int
         |    def bar: String
         |
         |class Concrete[T] extends Base[Int] {
         |
         |  extension (x: Int) override def foo: Int = ???
         |
         |  extension (x: Int) override def bar: String = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `given-object-creation` =
    checkEdit(
      """|package given
         |
         |trait Foo:
         |  def foo(x: Int): Int
         |  def bar(x: String): String
         |
         |given <<Foo>> with
         |  def foo(x: Int): Int = x
         |""".stripMargin,
      """|package given
         |
         |trait Foo:
         |  def foo(x: Int): Int
         |  def bar(x: String): String
         |
         |given Foo with
         |
         |  override def bar(x: String): String = ???
         |
         |  def foo(x: Int): Int = x
         |""".stripMargin
    )

  @Test def `given-object-creation-braces` =
    checkEdit(
      """|package given
         |
         |trait Foo:
         |  def foo(x: Int): Int
         |  def bar(x: String): String
         |
         |given <<Foo>> with {}
         |""".stripMargin,
      """|package given
         |
         |trait Foo:
         |  def foo(x: Int): Int
         |  def bar(x: String): String
         |
         |given Foo with {
         |
         |  override def foo(x: Int): Int = ???
         |
         |  override def bar(x: String): String = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `given-object-with` =
    checkEdit(
      """|package given
         |
         |trait Foo:
         |  def foo(x: Int): Int
         |  def bar(x: String): String
         |
         |given <<Foo>>
         |""".stripMargin,
      """|package given
         |
         |trait Foo:
         |  def foo(x: Int): Int
         |  def bar(x: String): String
         |
         |given Foo {
         |
         |  override def foo(x: Int): Int = ???
         |
         |  override def bar(x: String): String = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `type-alias` =
    checkEdit(
      """|package example
         |
         |trait NodeDb {
         |  type N
         |  def method(node: N): String
         |}
         |
         |class <<InMemoryNodeDb>> extends NodeDb
         |""".stripMargin,
      """|package example
         |
         |trait NodeDb {
         |  type N
         |  def method(node: N): String
         |}
         |
         |class InMemoryNodeDb extends NodeDb {
         |
         |  override def method(node: N): String = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `higher-kind-type` =
    checkEdit(
      """|package example
         |
         |trait NodeDb[F[_]]:
         |  type N
         |
         |  extension (node: N)
         |    def leftChild: F[Option[N]]
         |    def rightChild: F[Option[N]]
         |
         |class <<InMemoryNodeDb>>[F[_]] extends NodeDb[F]
         |""".stripMargin,
      """|package example
         |
         |trait NodeDb[F[_]]:
         |  type N
         |
         |  extension (node: N)
         |    def leftChild: F[Option[N]]
         |    def rightChild: F[Option[N]]
         |
         |class InMemoryNodeDb[F[_]] extends NodeDb[F] {
         |
         |  extension (node: N) override def leftChild: F[Option[N]] = ???
         |
         |  extension (node: N) override def rightChild: F[Option[N]] = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `path-dependent-type-arg` =
    checkEdit(
      """|package a
         |import scala.deriving.Mirror
         |trait Foo:
         |  def foo[A](using mirror: Mirror.ProductOf[A])(ordering: Ordering[mirror.MirroredElemTypes]): Unit
         |
         |class <<Bar>> extends Foo
         |""".stripMargin,
      """|package a
         |import scala.deriving.Mirror
         |import scala.deriving.Mirror.ProductOf
         |trait Foo:
         |  def foo[A](using mirror: Mirror.ProductOf[A])(ordering: Ordering[mirror.MirroredElemTypes]): Unit
         |
         |class Bar extends Foo {
         |
         |  override def foo[A](using mirror: ProductOf[A])(ordering: Ordering[mirror.MirroredElemTypes]): Unit = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `case-class` =
    checkEdit(
      """|package example
         |
         |sealed trait Demo {
         |  def implementMe: Int
         |}
         |
         |case class <<ADemo>>(value: Int) extends Demo
         |""".stripMargin,
      """|package example
         |
         |sealed trait Demo {
         |  def implementMe: Int
         |}
         |
         |case class ADemo(value: Int) extends Demo {
         |
         |  override def implementMe: Int = ???
         |
         |}
         |""".stripMargin
    )

  @Test def `end-marker` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base:
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |
         |  class <<Concrete>> extends Base:
         |
         |  end Concrete
         |
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base:
         |    def foo(x: Int): Int
         |    def bar(x: String): String
         |
         |  class Concrete extends Base:
         |
         |    override def foo(x: Int): Int = ???
         |
         |    override def bar(x: String): String = ???
         |
         |
         |  end Concrete
         |
         |}
         |""".stripMargin
    )

  @Test def `end-marker2` =
    checkEdit(
      """|package a
         |
         |object A {
         |  trait Base:
         |    def bar(x: String): String
         |
         |  class <<Concrete>>(x: Int, y: String) extends Base:
         |
         |  end Concrete
         |
         |}
         |""".stripMargin,
      """|package a
         |
         |object A {
         |  trait Base:
         |    def bar(x: String): String
         |
         |  class Concrete(x: Int, y: String) extends Base:
         |
         |    override def bar(x: String): String = ???
         |
         |
         |  end Concrete
         |
         |}
         |""".stripMargin
    )

  @Test def `braceless-case-class` =
    checkEdit(
      """|package a
         |
         |trait Base:
         |  def foo(x: Int): Int
         |  def bar(x: String): String
         |
         |case class <<Concrete>>() extends Base:
         |  def aaa = "aaa"
         |end Concrete
         |""".stripMargin,
      """|package a
         |
         |trait Base:
         |  def foo(x: Int): Int
         |  def bar(x: String): String
         |
         |case class Concrete() extends Base:
         |
         |  override def foo(x: Int): Int = ???
         |
         |  override def bar(x: String): String = ???
         |
         |  def aaa = "aaa"
         |end Concrete
         |""".stripMargin
    )

  def checkEdit(
      original: String,
      expected: String
  ): Unit =
    val edits = getAutoImplement(original)
    assertNonEmpty(edits, "Obtained no edits.")
    val (code, _, _) = params(original)
    val obtained = TextEdits.applyEdits(code, edits)
    assertNoDiff(expected, obtained)

  def getAutoImplement(
      original: String,
      filename: String = "A.scala"
  ): List[l.TextEdit] =
    val (code, _, offset) = params(original)
    val result = presentationCompiler
      .implementAbstractMembers(
        CompilerOffsetParams(URI.create(filename), code, offset, cancelToken)
      )
      .get()
    result.asScala.toList
