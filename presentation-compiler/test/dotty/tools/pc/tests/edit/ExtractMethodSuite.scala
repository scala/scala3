package dotty.tools.pc.tests.edit

import dotty.tools.pc.base.BaseExtractMethodSuite

import org.junit.Test

class ExtractMethodSuite extends BaseExtractMethodSuite:
  @Test def `simple-expr` =
    checkEdit(
      s"""|object A{
          |  val b = 4
          |  def method(i: Int) = i + 1
          |  @@val a = <<123 + method(b)>>
          |}""".stripMargin,
      s"""|object A{
          |  val b = 4
          |  def method(i: Int) = i + 1
          |  def newMethod(): Int =
          |    123 + method(b)
          |
          |  val a = newMethod()
          |}""".stripMargin
    )

  @Test def `no-param` =
    checkEdit(
      s"""|object A{
          |  def method(i: Int, j: Int) = i + j
          |  @@val a = {
          |    val c = 1
          |    <<val b = 2
          |    123 + method(b, 10)>>
          |  }
          |
          |}""".stripMargin,
      s"""|object A{
          |  def method(i: Int, j: Int) = i + j
          |  def newMethod(): Int =
          |    val b = 2
          |    123 + method(b, 10)
          |
          |  val a = {
          |    val c = 1
          |    newMethod()
          |  }
          |
          |}""".stripMargin
    )

  @Test def `single-param` =
    checkEdit(
      s"""|object A{
          |  def method(i: Int, j: Int) = i + j
          |  @@val a = {
          |    val c = 1
          |    <<val b = 2
          |    123 + method(c, 10)>>
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  def method(i: Int, j: Int) = i + j
          |  def newMethod(c: Int): Int =
          |    val b = 2
          |    123 + method(c, 10)
          |
          |  val a = {
          |    val c = 1
          |    newMethod(c)
          |  }
          |}""".stripMargin
    )

  @Test def `name-gen` =
    checkEdit(
      s"""|object A{
          |  def newMethod() = 1
          |  def newMethod0(a: Int) = a + 1
          |  def method(i: Int) = i + i
          |  @@val a = <<method(5)>>
          |}""".stripMargin,
      s"""|object A{
          |  def newMethod() = 1
          |  def newMethod0(a: Int) = a + 1
          |  def method(i: Int) = i + i
          |  def newMethod1(): Int =
          |    method(5)
          |
          |  val a = newMethod1()
          |}""".stripMargin
    )

  @Test def `multi-param` =
    checkEdit(
      s"""|object A{
          |  val c = 3
          |  def method(i: Int, j: Int) = i + 1
          |  @@val a = {
          |    val c = 5
          |    val b = 4
          |    <<123 + method(c, b) + method(b,c)>>
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  val c = 3
          |  def method(i: Int, j: Int) = i + 1
          |  def newMethod(b: Int, c: Int): Int =
          |    123 + method(c, b) + method(b,c)
          |
          |  val a = {
          |    val c = 5
          |    val b = 4
          |    newMethod(b, c)
          |  }
          |}""".stripMargin
    )

  @Test def `higher-scope` =
    checkEdit(
      s"""|object A{
          |  val b = 4
          |  def method(i: Int, j: Int, k: Int) = i + j + k
          |  val a = {
          |    @@def f() = {
          |      val c = 1
          |      <<val d = 3
          |      method(d, b, c)>>
          |    }
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  val b = 4
          |  def method(i: Int, j: Int, k: Int) = i + j + k
          |  val a = {
          |    def newMethod(c: Int): Int =
          |      val d = 3
          |      method(d, b, c)
          |
          |    def f() = {
          |      val c = 1
          |      newMethod(c)
          |    }
          |  }
          |}""".stripMargin
    )

  @Test def `match` =
    checkEdit(
      s"""|object A {
          |  @@val a = {
          |    val b = 4
          |    <<b + 2 match {
          |      case _ => b
          |    }>>
          |  }
          |}""".stripMargin,
      s"""|object A {
          |  def newMethod(b: Int): Int =
          |    b + 2 match {
          |      case _ => b
          |    }
          |
          |  val a = {
          |    val b = 4
          |    newMethod(b)
          |  }
          |}""".stripMargin
    )

  @Test def `nested-declarations` =
    checkEdit(
      s"""|object A {
          |  @@val a = {
          |    val c = 1
          |    <<val b = {
          |      val c = 2
          |      c + 1
          |    }
          |    c + 2>>
          |  }
          |}""".stripMargin,
      s"""|object A {
          |  def newMethod(c: Int): Int =
          |    val b = {
          |      val c = 2
          |      c + 1
          |    }
          |    c + 2
          |
          |  val a = {
          |    val c = 1
          |    newMethod(c)
          |  }
          |}""".stripMargin
    )

  @Test def `class-param` =
    checkEdit(
      s"""|object A{
          |  @@class B(val b: Int) {
          |    def f2 = <<b + 2>>
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  def newMethod(b: Int): Int =
          |    b + 2
          |
          |  class B(val b: Int) {
          |    def f2 = newMethod(b)
          |  }
          |}""".stripMargin
    )

  @Test def `method-param` =
    checkEdit(
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  @@def f1(a: Int) = {
          |    <<method(a)>>
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  def newMethod(a: Int): Int =
          |    method(a)
          |
          |  def f1(a: Int) = {
          |    newMethod(a)
          |  }
          |}""".stripMargin
    )

  @Test def `method-type` =
    checkEdit(
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  @@def f1[T](a: T) = {
          |    <<a>>
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  def newMethod[T](a: T): T =
          |    a
          |
          |  def f1[T](a: T) = {
          |    newMethod(a)
          |  }
          |}""".stripMargin
    )

  @Test def `method-type-no-param` =
    checkEdit(
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  @@def f1[T](a: T) = {
          |    <<Set.empty[T]>>
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  def newMethod[T](): Set[T] =
          |    Set.empty[T]
          |
          |  def f1[T](a: T) = {
          |    newMethod()
          |  }
          |}""".stripMargin
    )

  @Test def `inner-conflict` =
    checkEdit(
      s"""|object A{
          |  def method(i: Int, j: Int) = i + j
          |  @@val a = {
          |    val d = 3
          |    <<val b = {
          |      val d = 4
          |      d + 1
          |    }
          |    123 + method(b, 10)>>
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  def method(i: Int, j: Int) = i + j
          |  def newMethod(): Int =
          |    val b = {
          |      val d = 4
          |      d + 1
          |    }
          |    123 + method(b, 10)
          |
          |  val a = {
          |    val d = 3
          |    newMethod()
          |  }
          |}""".stripMargin
    )

  @Test def `extract-def` =
    checkEdit(
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  @@def f1(a: Int) = {
          |    def m2(b: Int) = b + 1
          |    <<method(2 + m2(a))>>
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  def newMethod(a: Int, m2: Int => Int): Int =
          |    method(2 + m2(a))
          |
          |  def f1(a: Int) = {
          |    def m2(b: Int) = b + 1
          |    newMethod(a, m2)
          |  }
          |}""".stripMargin
    )

  @Test def `extract-def-mult-params-lists` =
    checkEdit(
      s"""|object Hello {
          |  @@def m(): Unit = {
          |    def m2[T](a: T, j: Int)(i : Int) = List(a)
          |    val a = "aa"
          |    <<m2(a, 2)(2)>>
          |  }
          |}
          |""".stripMargin,
      s"""|object Hello {
          |  def newMethod[T](a: String, m2: (T, Int) => Int => List[T]): List[String] =
          |    m2(a, 2)(2)
          |
          |  def m(): Unit = {
          |    def m2[T](a: T, j: Int)(i : Int) = List(a)
          |    val a = "aa"
          |    newMethod(a, m2)
          |  }
          |}
          |""".stripMargin
    )

  @Test def `extract-def-mult-type-params` =
    checkEdit(
      s"""|object Hello {
          |  @@def m[T](a: T): Unit = {
          |    def m2[F](a: F, j: Int)(i : Int) = List(a)
          |    <<m2(a, 2)(2)>>
          |  }
          |}
          |""".stripMargin,
      s"""|object Hello {
          |  def newMethod[F, T](a: T, m2: (F, Int) => Int => List[F]): List[T] =
          |    m2(a, 2)(2)
          |
          |  def m[T](a: T): Unit = {
          |    def m2[F](a: F, j: Int)(i : Int) = List(a)
          |    newMethod(a, m2)
          |  }
          |}
          |""".stripMargin
    )

  @Test def `extract-def-partial` =
    checkEdit(
      s"""|object Hello {
          |  @@def m(): Unit = {
          |    def m2[T](a: T, j: Int)(i : Int) = List(a)
          |    <<m2("aa", 2)>>(2)
          |  }
          |}
          |""".stripMargin,
      s"""|object Hello {
          |  def newMethod[T](m2: (T, Int) => Int => List[T]): Int => List[String] =
          |    m2("aa", 2)
          |
          |  def m(): Unit = {
          |    def m2[T](a: T, j: Int)(i : Int) = List(a)
          |    newMethod(m2)(2)
          |  }
          |}
          |""".stripMargin
    )

  @Test def `extract-def-no-args` =
    checkEdit(
      s"""|object Hello {
          |  @@def m(): Unit = {
          |    def m2 = 9
          |    <<m2 + 3>>
          |  }
          |}
          |""".stripMargin,
      s"""|object Hello {
          |  def newMethod(m2: => Int): Int =
          |    m2 + 3
          |
          |  def m(): Unit = {
          |    def m2 = 9
          |    newMethod(m2)
          |  }
          |}
          |""".stripMargin
    )

  @Test def `extract-def-no-args2` =
    checkEdit(
      s"""|object Hello {
          |  @@def m(): Unit = {
          |    def m2() = 9
          |    <<m2() + 3>>
          |  }
          |}
          |""".stripMargin,
      s"""|object Hello {
          |  def newMethod(m2: () => Int): Int =
          |    m2() + 3
          |
          |  def m(): Unit = {
          |    def m2() = 9
          |    newMethod(m2)
          |  }
          |}
          |""".stripMargin
    )

  @Test def `extract-class` =
    checkEdit(
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  @@class Car(val color: Int) {
          |    def add(other: Car): Car = {
          |      <<new Car(other.color + color)>>
          |    }
          |  }
          |}""".stripMargin,
      s"""|object A{
          |  def method(i: Int) = i + 1
          |  def newMethod(color: Int, other: Car): Car =
          |    new Car(other.color + color)
          |
          |  class Car(val color: Int) {
          |    def add(other: Car): Car = {
          |      newMethod(color, other)
          |    }
          |  }
          |}""".stripMargin
    )

  @Test def `i6476` =
    checkEdit(
      """|object O {
         |  class C
         |  def foo(i: Int)(implicit o: C) = i
         |
         |  @@val o = {
         |    implicit val c = new C
         |    <<foo(2)>>
         |    ???
         |  }
         |}
         |""".stripMargin,
      """|object O {
         |  class C
         |  def foo(i: Int)(implicit o: C) = i
         |
         |  def newMethod()(given c: C): Int =
         |    foo(2)
         |
         |  val o = {
         |    implicit val c = new C
         |    newMethod()
         |    ???
         |  }
         |}
         |""".stripMargin
    )

  @Test def `i6476-2` =
    checkEdit(
      """|object O {
         |  class C
         |  def foo(i: Int)(implicit o: C) = i
         |
         |  @@val o = {
         |    <<foo(2)(new C)>>
         |    ???
         |  }
         |}
         |""".stripMargin,
      """|object O {
         |  class C
         |  def foo(i: Int)(implicit o: C) = i
         |
         |  def newMethod(): Int =
         |    foo(2)(new C)
         |
         |  val o = {
         |    newMethod()
         |    ???
         |  }
         |}
         |""".stripMargin
    )

  @Test def `i6476-3` =
    checkEdit(
      """|object O {
         |  class C
         |  class D
         |  def foo(i: Int)(using o: C)(x: Int)(using d: D) = i
         |
         |  @@val o = {
         |    given C = new C
         |    given D = new D
         |    val w = 2
         |    <<foo(w)(w)>>
         |    ???
         |  }
         |}
         |""".stripMargin,
      """|object O {
         |  class C
         |  class D
         |  def foo(i: Int)(using o: C)(x: Int)(using d: D) = i
         |
         |  def newMethod(w: Int)(given given_C: C, given_D: D): Int =
         |    foo(w)(w)
         |
         |  val o = {
         |    given C = new C
         |    given D = new D
         |    val w = 2
         |    newMethod(w)
         |    ???
         |  }
         |}
         |""".stripMargin
    )
