package dotty.tools.pc.tests.signaturehelp

import dotty.tools.pc.base.BaseSignatureHelpSuite

import org.junit.{ Ignore, Test }

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
         |Random(self: Random)
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
      """|File(x$0: URI)
         |     ^^^^^^^^
         |File(x$0: File, x$1: String)
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
      """|File(x$0: URI)
         |     ^^^^^^^^
         |File(x$0: File, x$1: String)
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
      """|apply[A](x: A | Null): Option[A]
         |         ^^^^^^^^^^^
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
      """|apply[A](x: A | Null): Option[A]
         |         ^^^^^^^^^^^
         |""".stripMargin
    )

  // https://github.com/scala/scala3/issues/15244
  @Test def `vararg` =
    check(
      """
        |object a {
        |  List(1, 2@@)
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
      """|apply[A](x: A | Null): Option[A]
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
        |  List[Int](1).lengthCompare(@@)
        |}
      """.stripMargin,
      """|lengthCompare(len: Int): Int
         |              ^^^^^^^^
         |lengthCompare(that: Iterable[?]): Int
         |""".stripMargin
    )

  @Ignore("See if applyCallInfo can still inform on lengthCompare's sig, even if recv is in error")
  @Test def `tparam5_TypeMismatch` =
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

  @Test def `tparam5_nonvarargs` =
    check(
      """
        |object a {
        |  Option[Int](1).getOrElse(@@)
        |}
      """.stripMargin,
      """|getOrElse[B >: Int](default: => B): B
         |                    ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Ignore("Similar to `tparam5_TypeMismatch`")
  @Test def `tparam5_nonvarargs_TypeMismatch` =
    check(
      """
        |object a {
        |  Option[String](1).getOrElse(@@)
        |}
      """.stripMargin,
      """|getOrElse[B >: String](default: => B): B
         |                       ^^^^^^^^^^^^^
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
      """|apply[A](x: A | Null): Option[A]
         |         ^^^^^^^^^^^
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
      """|to(end: Int): Inclusive
         |   ^^^^^^^^
         |to(end: Int, step: Int): Inclusive
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
      """|apply(viewId: String, nodeUri: String, label: String, [collapseState: String], [command: String], [icon: String], [tooltip: String]): TreeViewNode
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
      """|apply(viewId: String, nodeUri: String, label: String, [collapseState: String], [command: String]): TreeViewNode
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
      """|apply([age: Int], [name: String]): User
         |                  ^^^^^^^^^^^^^^
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
      """|user([street: Int], [name: String], [age: Int]): Int
         |     ^^^^^^^^^^^^^
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
      """|Catch[T](pf: Catcher[T], fin: Option[Finally], rethrow: Throwable => Boolean)
         |         ^^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `short-name1` =
    check(
      """
        |object A {
        |  new java.util.HashMap[String, Int]().computeIfAbsent(@@)
        |}
      """.stripMargin,
      // This is the correct result, as there is a conflict at Function: scala.Function and java.util.function.Function
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
      """|apply[A](x: A | Null): Option[A]
         |         ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `last-arg1` =
    check(
      """
        |object A {
        |  List[Int](1).map(a => @@)
        |}
      """.stripMargin,
      """|map[B](f: Int => B): List[B]
         |       ^^^^^^^^^^^
         |""".stripMargin
    )

  @Ignore("Similar to `tparam5_TypeMismatch`")
  @Test def `last-arg1_TypeMismatch` =
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

  @Ignore
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
      ""
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
         |                       ^^^^^^^^^^^
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
         |  List(1,2,3).test(s@@)""".stripMargin,
      """|test(x: Int): List[Int]
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `instantiated-type-var-old-ext-2` =
    check(
      """|object O:
         |  implicit class Test[T](xs: List[T]):
         |    def test(x: T): List[T] = ???
         |  List(1,2,3).test(@@)""".stripMargin,
      """|test(x: Int): List[Int]
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `instantiated-type-var-old-ext-3` =
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

  @Test def `instantiated-type-var-old-ext-4` =
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

  @Test def `instantiated-type-var-old-ext-5` =
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

  @Test def `multiline-before` =
    check(
      """|object Main {
         |  def deployment(
         |    fst: String,
         |    snd: Int = 1,
         |  ): Option[Int] = ???
         |  val abc = deployment(@@
         |    fst = "abc",
         |    snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment(fst: String, snd: Int): Option[Int]
         |           ^^^^^^^^^^^
         |""".stripMargin
     )

  @Test def `multiline-after-first` =
    check(
      """|object Main {
         |  def deployment(
         |    fst: String,
         |    snd: Int = 1,
         |  ): Option[Int] = ???
         |  val abc = deployment(
         |    fst = "abc", @@
         |    snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment(fst: String, snd: Int): Option[Int]
         |                        ^^^^^^^^
         |""".stripMargin
     )

  @Test def `multiline-between-first-and-second-a` =
    check(
      """|object Main {
         |  def deployment(
         |    fst: String,
         |    snd: Int = 1,
         |  ): Option[Int] = ???
         |  val abc = deployment(
         |    fst = "abc"
         |    @@
         |
         |    ,snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment(fst: String, snd: Int): Option[Int]
         |           ^^^^^^^^^^^
         |""".stripMargin
     )

  @Test def `multiline-between-first-and-second-b` =
    check(
      """|object Main {
         |  def deployment(
         |    fst: String,
         |    snd: Int = 1,
         |  ): Option[Int] = ???
         |  val abc = deployment(
         |    fst = "abc",
         |    @@
         |
         |    snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment(fst: String, snd: Int): Option[Int]
         |                        ^^^^^^^^
         |""".stripMargin
     )

  @Test def `multiline-end` =
    check(
      """|object Main {
         |  def deployment(
         |    fst: String,
         |    snd: Int = 1,
         |  ): Option[Int] = ???
         |  val abc = deployment(
         |    fst = "abc",
         |    snd = 1
         |  @@)
         |}
         |""".stripMargin,
      """|deployment(fst: String, snd: Int): Option[Int]
         |                        ^^^^^^^^
         |""".stripMargin
     )

  @Test def `type-var-multiline-before` =
    check(
      """|object Main {
         |  def deployment[A, B](
         |    fst: A,
         |    snd: B,
         |  ): Option[Int] = ???
         |  val abc = deployment[@@
             Int,
         |   String,
         |  ](
         |    fst = "abc",
         |    snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment[A, B](fst: A, snd: B): Option[Int]
         |           ^
         |""".stripMargin
     )

  @Test def `type-var-multiline-after` =
    check(
      """|object Main {
         |  def deployment[A, B](
         |    fst: A,
         |    snd: B,
         |  ): Option[Int] = ???
         |  val abc = deployment[
         |   Int, @@
         |   String,
         |  ](
         |    fst = "abc",
         |    snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment[A, B](fst: A, snd: B): Option[Int]
         |              ^
         |""".stripMargin
     )

  @Test def `type-var-multiline-between-first-and-second-a` =
    check(
      """|object Main {
         |  def deployment[A, B](
         |    fst: A,
         |    snd: B,
         |  ): Option[Int] = ???
         |  val abc = deployment[
         |   Int
         |   @@
         |
         |   ,String
         |  ](
         |    fst = "abc",
         |    snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment[A, B](fst: A, snd: B): Option[Int]
         |           ^
         |""".stripMargin
     )

  @Test def `type-var-multiline-between-first-and-second-b` =
    check(
      """|object Main {
         |  def deployment[A, B](
         |    fst: A,
         |    snd: B,
         |  ): Option[Int] = ???
         |  val abc = deployment[
         |   Int,
         |   @@
         |
         |   String,
         |  ](
         |    fst = "abc",
         |    snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment[A, B](fst: A, snd: B): Option[Int]
         |              ^
         |""".stripMargin
     )

  @Test def `type-var-multiline-end` =
    check(
      """|object Main {
         |  def deployment[A, B](
         |    fst: A,
         |    snd: B,
         |  ): Option[Int] = ???
         |  val abc = deployment[
         |     String,
         |     Int,
         |  @@](
         |    fst = "abc",
         |    snd = 1
         |  )
         |}
         |""".stripMargin,
      """|deployment[A, B](fst: A, snd: B): Option[Int]
         |              ^
         |""".stripMargin
     )

  @Test def `dont-show-directly-after-parenthesis` =
    check(
      """|object Main {
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, 2)@@
         |}
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-directly-after-parenthesis-2` =
    check(
      """|object Main {
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, 2)@@
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-directly-when-unclosed` =
    check(
      """|object Main {
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, (2 + 1)@@
         |}
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-after-parenthesis-1` =
    check(
      """|object Main {
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, 2) @@
         |}
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-after-parenthesis-2` =
    check(
      """|object Main {
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, 2)  @@
         |}
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-after-parenthesis-newline` =
    check(
      """|object Main {
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, 2)
         |@@
         |}
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-after-parenthesis-newline-last-statement` =
    check(
      """|object Main:
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, 2)
         |
         |@@
         |
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-after-parenthesis-newline-last-statement-unclosed-1` =
    check(
      """|object Main:
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, 2
         |
         |@@
         |
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-after-parenthesis-newline-last-statement-unclosed-2` =
    check(
      """|object Main:
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, (1 + 2)
         |
         |@@
         |
         |""".stripMargin,
      ""
     )

  @Test def `dont-show-after-parenthesis-unclosed-2` =
    check(
      """|object Main {
         |  def test(a: Int, b: Int): Int = ???
         |  test(1, 2
         |
         |  @@
         |}
         |""".stripMargin,
       ""
     )

  @Test def `select-arg-detection` =
    check(
      """|object Main:
         |  object Foo:
         |    case class Test(x: Int)
         |  def test(a: Foo.Test, b: Foo.Test): Int = ???
         |  test(Foo.Test(1), @@)
         |""".stripMargin,
      """|test(a: Test, b: Test): Int
         |              ^^^^^^^
         |""".stripMargin
    )

  @Test def `singature-help-works-in-select` =
    check(
      """|object Main:
         |  object Foo:
         |    class Test(x: Int, y: Int)
         |  new Foo.Test(1, @@)
         |""".stripMargin,
      """|Test(x: Int, y: Int)
         |             ^^^^^^
         |""".stripMargin
    )

  @Test def `curried-help-works-in-select` =
    check(
      """|object Main:
         |  def test(xxx: Int, yyy: Int)(zzz: Int): Int = ???
         |  test(yyy = 5, xxx = 7)(@@)
         |""".stripMargin,
      """|test([yyy: Int], [xxx: Int])(zzz: Int): Int
         |                             ^^^^^^^^
         |""".stripMargin
    )

  @Test def `no-signature-help-for-parameterless-method` =
    check(
      """|object Main:
         |  def test: Int = ???
         |  test(@@)
         |""".stripMargin,
      ""
    )

  @Test def `show-methods-returning-tuples` =
    check(
      """|object Main:
         |  def test(): (Int, Int) = ???
         |  test(@@)
         |""".stripMargin,
      "test(): (Int, Int)"
    )

  @Test def `show-methods-returning-tuples-2` =
    check(
      """|object Main:
         |  def test(x: Int): (Int, Int) = ???
         |  test(@@)
         |""".stripMargin,
      """|test(x: Int): (Int, Int)
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `dont-show-tuples-application` =
    check(
      """|object Main:
         |  (1, @@)
         |""".stripMargin,
      ""
    )

  @Test def `type-param-start` =
    check(
      """|object Main:
         |  def test[A](x: A): A = ???
         |  test[@@]
         |""".stripMargin,
      """|test[A](x: A): A
         |     ^
         |""".stripMargin
    )

  @Test def `error-recovery-1` =
    check(
      """|object Main:
         |  def test[A](x: A): Foo[A] = ???
         |  test[@@]
         |""".stripMargin,
      """|test[A](x: A): Foo[A]
         |     ^
         |""".stripMargin
    )

  @Test def `error-recovery-2` =
    check(
      """|object Main:
         |  def test[A](x: A): Foo[A] = ???
         |  test[Int](@@)
         |""".stripMargin,
      """|test[A](x: A): Foo[A]
         |        ^^^^
         |""".stripMargin
    )

  @Test def `type-param-shortening` =
    check(
      """|object M:
         |  def test[T <: java.io.File](x: Int): Int = ???
         |  test(@@)
         |""".stripMargin,
      """|test[T <: File](x: Int): Int
         |                ^^^^^^
         |""".stripMargin
    )

  @Test def `implicit-param` =
    check(
      """|object M:
         |  trait Context
         |  def test(x: Int)(using ctx: Context): Int = ???
         |  test(@@)
         |""".stripMargin,
      """|test(x: Int)(using ctx: Context): Int
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `context-param` =
    check(
      """|object M:
         |  def test(x: Int, y: Int = 7)(z: Int ?=> Int): Int = ???
         |  test(@@)
         |""".stripMargin,
      """|test(x: Int, y: Int)(z: (Int) ?=> Int): Int
         |     ^^^^^^
         |""".stripMargin
    )

  @Test def `empty-implicit-params` =
    check(
      """|object M:
         |  def test(x: Int)(using String): Int = ???
         |  test(1)(@@)
         |""".stripMargin,
      """|test(x: Int)(using String): Int
         |                   ^^^^^^
         |""".stripMargin
    )

  @Test def `multiple-implicits-1` =
    check(
      """|object M:
         |  def a(using Int)(using String): Int = ???
         |  a(@@)
         |""".stripMargin,
      """|a(using Int)(using String): Int
         |        ^^^
         |""".stripMargin
    )


  @Test def `multiple-implicits-2` =
    check(
      """|object M:
         |  def a(using Int)(using String): Int = ???
         |  a(using 5)(@@)
         |""".stripMargin,
      """|a(using Int)(using String): Int
         |                   ^^^^^^
         |""".stripMargin
    )

  @Test def `multiple-implicits-3` =
    check(
      """|object M:
         |  def a(using Int)(using String)(x: Int): Int = ???
         |  a(@@)
         |""".stripMargin,
      """|a(using Int)(using String)(x: Int): Int
         |        ^^^
         |""".stripMargin
    )

  @Test def `multiple-implicits-4` =
    check(
      """|object M:
         |  def a(using Int)(using String)(x: Int): Int = ???
         |  a(using 5)(@@)
         |""".stripMargin,
      """|a(using Int)(using String)(x: Int): Int
         |                   ^^^^^^
         |""".stripMargin
    )

  @Test def `multiple-implicits-error-1` =
    check(
      """|object M:
         |  def a(using Int)(using String)(x: Int): Int = ???
         |  a(5)(@@)
         |""".stripMargin,
      """|
         |a(using Int)(using String)(x: Int): Int
         |                   ^^^^^^
         |""".stripMargin
    )

  @Test def `multiple-implicits-error-2` =
    check(
      """|object M:
         |  def a(using Int)(using String)(x: Int): Int = ???
         |  a(5)(@@)
         |""".stripMargin,
      """|a(using Int)(using String)(x: Int): Int
         |                   ^^^^^^
         |""".stripMargin
    )

  @Test def `dont-crash-at-last-position` =
    check(
      """|object M:
         |  def test(x: Int): Int = ???
         |  test(@@""".stripMargin,
      ""
    )

  @Test def `type-var-position` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[A@@, C]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |     ^
         |""".stripMargin
    )

  @Test def `type-var-position-1` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[@@A, C]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |     ^
         |""".stripMargin
    )

  @Test def `type-var-position-2` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[A@@
         |  , C]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |     ^
         |""".stripMargin
    )

  @Test def `type-var-position-3` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[A, C@@]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |        ^
         |""".stripMargin
    )

  @Test def `type-var-position-4` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[A,@@ C]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |        ^
         |""".stripMargin
    )

  @Test def `type-var-position-5` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[A, @@C]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |        ^
         |""".stripMargin
    )

  @Test def `type-var-position-6` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[
         |    A@@,
         |    C
         |  ]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |     ^
         |""".stripMargin
    )

  @Test def `type-var-position-7` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[
         |    A,
         |    C@@
         |  ]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |        ^
         |""".stripMargin
     )

  @Test def `type-var-position-8` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[
         |    A,
         |    @@C
         |  ]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |        ^
         |""".stripMargin
     )

  @Test def `type-var-position-9` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[
         |    A,
         |    C
         |  @@]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |        ^
         |""".stripMargin
     )

  @Test def `type-var-position-10` =
    check(
      """|trait Test[A, B]:
         |  def doThing[C](f: B => Test[@@
         |    A,
         |    C
         |  ]) = ???
         |""".stripMargin,
      """|Test[A, B]: Test
         |     ^
         |""".stripMargin
     )

  @Test def `correct-alternative` =
    check(
      """
        |object x {
        |  def foo(i: Int, s: String): Unit = ???
        |  def foo(i: Boolean, s: Int, x: Double): Unit = ???
        |
        |  foo(false, @@)
        |}
        |""".stripMargin,
      """
        |foo(i: Boolean, s: Int, x: Double): Unit
        |                ^^^^^^
        |foo(i: Int, s: String): Unit
        |""".stripMargin
    )

  @Test def `correct-alternative1` =
    check(
      """
        |object x {
        |  def foo(i: Boolean, s: String)(b: Int): Unit = ???
        |  def foo(i: Boolean, s: Int)(b: String): Unit = ???
        |
        |  foo(false, 123)(@@)
        |}
        |""".stripMargin,
      """
        |foo(i: Boolean, s: Int)(b: String): Unit
        |                        ^^^^^^^^^
        |foo(i: Boolean, s: String)(b: Int): Unit
        |""".stripMargin
    )

  @Test def `proper-param-empty-list` =
    check(
      """
        |object x {
        |  def foo[K, V](): Unit = ???
        |  foo(@@)
        |}
        |""".stripMargin,
      "foo[K, V](): Unit"
    )

  @Test def `proper-param-list-after-param-empty-list` =
    check(
      """
        |object x {
        |  def foo[K, V]()(x: Int): Unit = ???
        |  foo()(@@)
        |}
        |""".stripMargin,
      """
      |foo[K, V]()(x: Int): Unit
      |            ^^^^^^
      """.stripMargin
    )

  @Test def `proper-function-signature` =
    check(
      """
        |object OOO {
        |
        |val function: (x: Int, y: String) => Unit =
        |(_, _) =>
        | ()
        |
        |function(@@, "one")
        |}
      """.stripMargin,
      """|apply(v1: Int, v2: String): Unit
         |      ^^^^^^^
         |""".stripMargin
    )

  @Test def `proper-function-signature-with-explicit-apply` =
    check(
      """
        |object OOO {
        |
        |val function: (x: Int, y: String) => Unit =
        |(_, _) =>
        | ()
        |
        |function.apply(@@, "one")
        |}
      """.stripMargin,
      """|apply(v1: Int, v2: String): Unit
         |      ^^^^^^^
         |""".stripMargin
    )


  @Test def `opaque-type-parameter` =
    check(
      """|object History {
         |  opaque type Builder[A] = String
         |  def build(b: Builder[Unit]): Int = ???
         |}
         |object Main {
         |  History.build(@@)
         |}
         |""".stripMargin,
      """|build(b: Builder[Unit]): Int
         |      ^^^^^^^^^^^^^^^^
         |""".stripMargin
    )
