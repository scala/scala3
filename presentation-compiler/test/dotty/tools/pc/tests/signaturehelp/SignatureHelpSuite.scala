package dotty.tools.pc.tests.signaturehelp

import dotty.tools.pc.base.BaseSignatureHelpSuite

import org.junit.Test

class SignatureHelpSuite extends BaseSignatureHelpSuite:

  @Test def `method` =
    check(
      """
        |object a {
        |  assert(true, ms@@)
        |}
      """.stripMargin,
      """|assert(assertion: Boolean): Unit
         |assert(assertion: Boolean, message: => Any): Unit
         |                           ^^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `empty` =
    check(
      """
        |object a {
        |  assert(@@)
        |}
      """.stripMargin,
      """|assert(assertion: Boolean): Unit
         |       ^^^^^^^^^^^^^^^^^^
         |assert(assertion: Boolean, message: => Any): Unit
         |""".stripMargin
    )

  @Test def `erroneous` =
    check(
      """
        |object a {
        |  Option(1).fold("")(_ => a@@)
        |}
      """.stripMargin,
      """|fold[B](ifEmpty: => B)(f: Int => B): B
         |                       ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `canbuildfrom2` =
    check(
      """
        |object a {
        |  List(1).map(@@)
        |}
      """.stripMargin,
      """|map[B](f: Int => B): List[B]
         |       ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `ctor` =
    check(
      """
        |object a {
        |  val random = new scala.util.Random(@@)
        |}
      """.stripMargin,
      """|Random()
         |Random(seed: Int)
         |Random(seed: Long)
         |Random(self: java.util.Random)
         |""".stripMargin
    )

  @Test def `ctor1` =
    check(
      """
        |object a {
        |  new ProcessBuilder(@@)
        |}
      """.stripMargin,
      """|ProcessBuilder(x$0: String*)
         |               ^^^^^^^^^^^^
         |ProcessBuilder(x$0: java.util.List[String])
         |""".stripMargin
    )

  @Test def `ctor2` =
    check(
      """
        |object a {
        |  new Some(10@@)
        |}
      """.stripMargin,
      """|Some[A](value: A)
         |        ^^^^^^^^
         |""".stripMargin
    )

  @Test def `ctor3` =
    check(
      """
        |object a {
        |  import java.io.File
        |  new File(@@)
        |}
      """.stripMargin,
      """|File(x$0: java.net.URI)
         |     ^^^^^^^^^^^^^^^^^
         |File(x$0: java.io.File, x$1: String)
         |File(x$0: String, x$1: String)
         |File(x$0: String)
         |""".stripMargin
    )

  @Test def `ctor4` =
    check(
      """
        |object a {
        |  new java.io.File(@@)
        |}
                 """.stripMargin,
      """|File(x$0: java.net.URI)
         |     ^^^^^^^^^^^^^^^^^
         |File(x$0: java.io.File, x$1: String)
         |File(x$0: String, x$1: String)
         |File(x$0: String)
         |""".stripMargin
    )

  @Test def `apply` =
    check(
      """
        |object a {
        |  def apply(a: Int): Int = a
        |  def apply(b: String): String = b
        |  a(""@@)
        |}
      """.stripMargin,
      """|apply(b: String): String
         |      ^^^^^^^^^
         |apply(a: Int): Int
         |""".stripMargin
    )

  @Test def `partial` =
    check(
      """
        |object a {
        |  Option(1).collect {
        |   case@@
        |  }
        |}
      """.stripMargin,
      """|collect[B](pf: PartialFunction[Int, B]): Option[B]
         |           ^^^^^^^^^^^^^^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `nested` =
    check(
      """
        |object a {
        |  List(Option(1@@))
        |}
      """.stripMargin,
      """|apply[A](x: A): Option[A]
         |         ^^^^
         |""".stripMargin
    )

  @Test def `nested2` =
    check(
      """
        |object a {
        |  List(Opt@@ion(1))
        |}
      """.stripMargin,
      """|apply[A](elems: A*): List[A]
         |         ^^^^^^^^^
         |""".stripMargin
    )

  @Test def `nested3` =
    check(
      """
        |object a {
        |  List(Option(@@))
        |}
      """.stripMargin,
      """|apply[A](x: A): Option[A]
         |         ^^^^
         |""".stripMargin
    )

  // https://github.com/lampepfl/dotty/issues/15244
  @Test def `vararg` =
    check(
      """
        |object a {
        |  List(1, 2@@
        |}
    """.stripMargin,
      """|apply[A](elems: A*): List[A]
         |         ^^^^^^^^^
         |""".stripMargin
    )

  @Test def `tparam` =
    check(
      """
        |object a {
        |  identity[I@@]
        |}
      """.stripMargin,
      """|identity[A](x: A): A
         |         ^
         |""".stripMargin
    )

  @Test def `tparam2` =
    check(
      """
        |object a {
        |  Option.empty[I@@]
        |}
      """.stripMargin,
      """|empty[A]: Option[A]
         |      ^
         |""".stripMargin
    )

  @Test def `tparam3` =
    check(
      """
        |object a {
        |  Option[I@@]
        |}
      """.stripMargin,
      """|apply[A](x: A): Option[A]
         |      ^
         |""".stripMargin
    )

  @Test def `tparam4` =
    check(
      """
        |object a {
        |  Map.empty[I@@]
        |}
      """.stripMargin,
      """|empty[K, V]: Map[K, V]
         |      ^
         |""".stripMargin
    )

  @Test def `tparam5` =
    check(
      """
        |object a {
        |  List[String](1).lengthCompare(@@)
        |}
      """.stripMargin,
      """|lengthCompare(len: Int): Int
         |              ^^^^^^^^
         |lengthCompare(that: Iterable[?]): Int
         |""".stripMargin
    )

  @Test def `error1` =
    check(
      """
        |object a {
        |  Map[Int](1 @@-> "").map {
        |  }
        |}
      """.stripMargin,
      """|apply[K, V](elems: (K, V)*): Map[K, V]
         |            ^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `for` =
    check(
      """
        |object a {
        |  for {
        |    i <- Option(1)
        |    j < 1.to(i)
        |    if i > j
        |    k@@ = i + j
        |    l <- j.to(k)
        |  } yield l
        |}
      """.stripMargin,
      ""
    )

  @Test def `for1` =
    check(
      """
        |object a {
        |  for {
        |    i <- List(1)
        |    k = {
        |      Option(10@@)
        |    }
        |  } yield k
        |}
      """.stripMargin,
      """|apply[A](x: A): Option[A]
         |         ^^^^
         |""".stripMargin
    )

  @Test def `for2` =
    check(
      """
        |object a {
        |  for {
        |    i <- List(1)
        |    if i < 0
        |    k = 100
        |    j <- i.to(@@)
        |  } yield k
        |}
      """.stripMargin,
      """|to(end: Int): scala.collection.immutable.Range.Inclusive
         |   ^^^^^^^^
         |to(end: Int, step: Int): scala.collection.immutable.Range.Inclusive
         |""".stripMargin,
      stableOrder = false
    )

  @Test def `bounds` =
    check(
      """
        |object a {
        |  Map.empty[Int, String].applyOrElse(1@@)
        |}
      """.stripMargin,
      """|applyOrElse[K1 <: Int, V1 >: String](x: K1, default: K1 => V1): V1
         |                                     ^^^^^
         |""".stripMargin
    )

  @Test def `error` =
    check(
      """
        |object a {
        |  Map[Int](1 @@-> "").map {
        |  }
        |}
      """.stripMargin,
      """|apply[K, V](elems: (K, V)*): Map[K, V]
         |            ^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `case-class` =
    check(
      """
        |import java.{util => ju}
        |import javax.annotation.Nullable
        |
        |
        |case class TreeViewNode(
        |    viewId: String,
        |    @Nullable nodeUri: String,
        |    label: String,
        |    @Nullable command: String = null,
        |    @Nullable icon: String = null,
        |    @Nullable tooltip: String = null,
        |    // One of "collapsed", "expanded" or "none"
        |    @Nullable collapseState: String = null,
        |)
        |
        |object O{
        |  val viewId = ""
        |  val rootUri = ""
        |  val title = ""
        |  def root: TreeViewNode =
        |    TreeViewNode(
        |      vi@@ewId,
        |      rootUri,
        |      title,
        |      collapseState = "",
        |    )
        |}
        |
        |
      """.stripMargin,
      """|apply(viewId: String, nodeUri: String, label: String, command: String, icon: String, tooltip: String, collapseState: String): TreeViewNode
         |      ^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `case-class2` =
    check(
      """
        |import java.{util => ju}
        |import javax.annotation.Nullable
        |
        |
        |case class TreeViewNode(
        |    viewId: String,
        |    @Nullable nodeUri: String,
        |    label: String,
        |    @Nullable command: String = null,
        |    // One of "collapsed", "expanded" or "none"
        |    @Nullable collapseState: String = null,
        |)
        |
        |object O{
        |  def root: TreeViewNode =
        |    val viewId = ""
        |    val rootUri = ""
        |    val title = ""
        |    TreeViewNode(
        |      vi@@ewId,
        |      rootUri,
        |      title,
        |      collapseState = "",
        |    )
        |}
        |
        |
      """.stripMargin,
      """|apply(viewId: String, nodeUri: String, label: String, command: String, collapseState: String): TreeViewNode
         |      ^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named` =
    check(
      """
        |case class User(name: String = "John", age: Int = 42)
        |object A {
        |  User(age = 1, @@)
        |}
      """.stripMargin,
      """|apply(name: String, age: Int): User
         |                    ^^^^^^^^
         |""".stripMargin
    )

  @Test def `named1` =
    check(
      """
        |case class User(name: String = "John", age: Int = 42)
        |object A {
        |  User(name = "", @@)
        |}
      """.stripMargin,
      """|apply(name: String, age: Int): User
         |                    ^^^^^^^^
         |""".stripMargin
    )

  @Test def `named2` =
    check(
      """
        |object A {
        |  def user(name: String, age: Int) = age
        |  user(na@@me = "", age = 42)
        |}
      """.stripMargin,
      """|user(name: String, age: Int): Int
         |     ^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named3` =
    check(
      """
        |object A {
        |  def user(name: String, age: Int): Int = age
        |  def user(name: String, age: Int, street: Int): Int = age
        |  def x = user(str@@eet = 42, name = "", age = 2)
        |}
      """.stripMargin,
      """|user(name: String, age: Int, street: Int): Int
         |                             ^^^^^^^^^^^
         |user(name: String, age: Int): Int
         |""".stripMargin
    )

  @Test def `named4` =
    check(
      """
        |object A {
        |  identity(x = @@)
        |}
      """.stripMargin,
      """|identity[A](x: A): A
         |            ^^^^
         |""".stripMargin
    )

  @Test def `short-name` =
    check(
      """
        |object A {
        |  new scala.util.control.Exception.Catch(@@)
        |}
      """.stripMargin,
      // TODO short names are not supported yet
      """|Catch[T](pf: scala.util.control.Exception.Catcher[T], fin: Option[scala.util.control.Exception.Finally], rethrow: Throwable => Boolean)
         |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `short-name1` =
    check(
      """
        |object A {
        |  new java.util.HashMap[String, Int]().computeIfAbsent(@@)
        |}
      """.stripMargin,
      // TODO short names are not supported yet
      """|computeIfAbsent(x$0: String, x$1: java.util.function.Function[? >: String, ? <: Int]): Int
         |                ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `curry5` =
    check(
      """
        |object a {
        |  def curry(a: Int)(c: Int) = a
        |  curry(1)(3@@)
        |}
      """.stripMargin,
      """|
         |curry(a: Int)(c: Int): Int
         |              ^^^^^^
         |""".stripMargin
    )

  @Test def `last-arg` =
    check(
      """
        |object A {
        |  Option(a @@)
        |}
      """.stripMargin,
      """|apply[A](x: A): Option[A]
         |         ^^^^
         |""".stripMargin
    )

  @Test def `last-arg1` =
    check(
      """
        |object A {
        |  List[Int]("").map(a => @@)
        |}
      """.stripMargin,
      """|map[B](f: Int => B): List[B]
         |       ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `last-arg2` =
    check(
      """
        |object A {
        |  List(1).map(a => 2 @@)
        |}
      """.stripMargin,
      """|map[B](f: Int => B): List[B]
         |       ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `last-arg3` =
    check(
      """
        |  trait TypeClass[F[_]]
        |  object App {
        |    final class TypeClassOps[F[_], A](private val a: F[A]) extends AnyVal {
        |      def map[G[_]](fn: A => G[A])(implicit T: TypeClass[F]): G[A] = ???
        |    }
        |    implicit def conv[F[A], A](e: F[A])(implicit T: TypeClass[F]): TypeClassOps[F, A] = new TypeClassOps(e)
        |    class App[F[_]:TypeClass] {
        |      null.asInstanceOf[F[Int]].map(a => @@)
        |    }
        |  }
      """.stripMargin,
      """|map[G[_$3]](fn: Int => G[Int])(using T: TypeClass[F]): G[Int]
         |            ^^^^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `evidence` =
    check(
      """
        |object a {
        |  Array.empty[@@]
        |}
      """.stripMargin,
      """|empty[T: ClassTag]: Array[T]
         |      ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `implicit-conv` =
    check(
      """
        |case class Text[T](value: T)
        |object Text {
        |  implicit def conv[T](e: T): Text[T] = Text(e)
        |}
        |object a {
        |  def foo[T](e: Text[T]): T = e.value
        |  foo(4@@2)
        |}
      """.stripMargin,
      """|conv[T](e: T): Text[T]
         |        ^^^^
         |""".stripMargin
    )

  @Test def `type` =
    check(
      """
        |object a {
        |  val x: Map[Int, Stri@@ng]
        |}
      """.stripMargin,
      """|Map[K, V]: Map
         |       ^
         |""".stripMargin
    )

  @Test def `type1` =
    check(
      """
        |object a {
        |  val x: Map[Int, Stri@@]
        |}
      """.stripMargin,
      """|Map[K, V]: Map
         |       ^
         |""".stripMargin
    )

  @Test def `off-by-one` =
    check(
      """
        |object a {
        |  identity(42)@@
        |}
        |""".stripMargin,
      """|identity[A](x: A): A
         |            ^^^^
         |""".stripMargin
    )

  @Test def `off-by-one2` =
    check(
      """
        |object a {
        |  identity(42@@)
        |}
        |""".stripMargin,
      """|identity[A](x: A): A
         |            ^^^^
         |""".stripMargin
    )

  @Test def `between-parens` =
    check(
      """
        |object a {
        |  Option(1).fold(2)@@(_ + 1)
        |}
        |""".stripMargin,
      """|fold[B](ifEmpty: => B)(f: Int => B): B
         |        ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `between-parens2` =
    check(
      """
        |object a {
        |  Option(1).fold(2@@)(_ + 1)
        |}
        |""".stripMargin,
      """|fold[B](ifEmpty: => B)(f: Int => B): B
         |        ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `between-parens3` =
    check(
      """
        |object a {
        |  Option(1).fold(2)(@@_ + 1)
        |}
        |""".stripMargin,
      """|fold[B](ifEmpty: => B)(f: Int => B): B
         |                       ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `local-method` =
    check(
      """
        |object Main {
        |  def foo() = {
        |    def deployment(
        |      fst: String,
        |      snd: Int = 1,
        |    ): Option[Int] = ???
        |    val abc = deployment(@@)
        |  }
        |}
        |""".stripMargin,
      """|deployment(fst: String, snd: Int): Option[Int]
         |           ^^^^^^^^^^^
         |""".stripMargin,
    )

  @Test def `local-method2` =
      check(
        """
          |object Main {
          |  val foo = {
          |    def deployment(
          |      fst: String,
          |      snd: Int = 1,
          |    ): Option[Int] = ???
          |    deployment(@@)
          |  }
          |}
          |""".stripMargin,
        """|deployment(fst: String, snd: Int): Option[Int]
           |           ^^^^^^^^^^^
           |""".stripMargin,
      )

  @Test def `local-method3` =
    check(
      """
        |object Main {
        |  def foo = {
        |    object a {
        |      def apply(a: Int): Int = a
        |      def apply(b: String): String = b
        |      a(""@@)
        |    }
        |  }
        |}
        |""".stripMargin,
      """|apply(b: String): String
         |      ^^^^^^^^^
         |apply(a: Int): Int
         |""".stripMargin
    )

  @Test def `instantiated-type-var-old-ext-1` =
    check(
      """|object O:
         |  implicit class Test[T](xs: List[T]):
         |    def test(x: T): List[T] = ???
         |  List(1,2,3).test(@@""".stripMargin,
      """|test(x: Int): List[Int]
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `instantiated-type-var-old-ext-2` =
    check(
      """|object O:
         |  implicit class Test[T](xs: List[T]):
         |    def test(x: T): List[T] = ???
         |  List(1,2,3).test(s@@""".stripMargin,
      """|test(x: Int): List[Int]
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `instantiated-type-var-old-ext-3` =
    check(
      """|object O:
         |  implicit class Test[T](xs: List[T]):
         |    def test(x: T): List[T] = ???
         |  List(1,2,3).test(@@)""".stripMargin,
      """|test(x: Int): List[Int]
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `instantiated-type-var-old-ext-4` =
    check(
      """|object O:
         |  implicit class Test[T](xs: List[T]):
         |    def test(x: T): List[T] = ???
         |  List(1,2,3).test(
         |    @@
         |  )""".stripMargin,
      """|test(x: Int): List[Int]
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `instantiated-type-var-old-ext-5` =
    check(
      """|object O:
         |  implicit class Test[T](xs: List[T]):
         |    def test(x: T): List[T] = ???
         |  List(1,2,3).test(
         |    @@
         |  println("test")
         |""".stripMargin,
      """|test(x: Int | Unit): List[Int | Unit]
         |     ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `instantiated-type-var-old-ext-6` =
    check(
      """|object O:
         |  implicit class Test[T](xs: List[T]):
         |    def test(y: String, x: T): List[T] = ???
         |  List(1,2,3).test("", @@)
         |""".stripMargin,
      """|test(y: String, x: Int): List[Int]
         |                ^^^^^^
         |""".stripMargin
    )
