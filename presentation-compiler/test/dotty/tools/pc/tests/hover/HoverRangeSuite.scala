package dotty.tools.pc.tests.hover

import dotty.tools.pc.base.BaseHoverSuite

import org.junit.Test

class HoverRangeSuite extends BaseHoverSuite:

  @Test def `range-sum-method` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: Int = {
         |    val l = List(1,2,3)
         |    <<l.map { x =>
         |      x.to(x*x)
         |        .flatMap { y =>
         |          List(y + 2137)
         |        }
         |      .sum
         |    }.%<%sum%>%>>
         |  }
         |}
         |""".stripMargin,
      """|Int
         |def sum[B >: Int](implicit num: Numeric[B]): B""".stripMargin.hoverRange
    )

  @Test def `range-sum-method-generic` =
    check(
      """|package helpers
         |
         |class XDClass[T] {
         |  def xd: T = {
         |    val l: List[T] = ???
         |    <<l.%<%head%>%>>
         |  }
         |}
         |""".stripMargin,
      """|T
         |def head: T""".stripMargin.hoverRange
    )

  @Test def `range-longer-expression` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: Int = {
         |    val l = List(1,2,3)
         |    <<%<%l.map { x =>
         |      x.to(x*x)
         |        .flatMap { y =>
         |          List(y + 2137)
         |        }
         |      .sum
         |    }.sum%>%>>
         |  }
         |}
         |""".stripMargin,
      """|Int
         |def sum[B >: Int](implicit num: Numeric[B]): B""".stripMargin.hoverRange
    )

  @Test def `range-longer-expression-1` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: Int = {
         |    val l = List(1,2,3)
         |    l.map { x =>
         |      <<%<%x.to(x*x)
         |        .flatMap { y =>
         |          List(y + 2137)
         |        }%>%>>
         |      .sum
         |    }.sum
         |  }
         |}
         |""".stripMargin,
      """|IndexedSeq[Int]
         |override def flatMap[B](f: Int => IterableOnce[B]): IndexedSeq[B]""".stripMargin.hoverRange
    )

  @Test def `range-expression-in-closure` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: Int = {
         |    val l = List(1,2,3)
         |    l.map { x =>
         |      x.to(x*x)
         |        .flatMap { y =>
         |          <<%<%List(y + 2137)%>%>>
         |        }
         |      .sum
         |    }.sum
         |  }
         |}
         |""".stripMargin,
      """|List[Int]
         |def apply[A](elems: A*): List[A]""".stripMargin.hoverRange
    )

  @Test def `range-lfs-of-valdef` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: Int = {
         |    <<val %<%l%>% = List(1,2,3)>>
         |    l.map { x =>
         |      x.to(x*x)
         |        .flatMap { y =>
         |          List(y + 2137)
         |        }
         |      .sum
         |    }.sum
         |  }
         |}
         |""".stripMargin,
      """|List[Int]
         |val l: List[Int]""".stripMargin.hoverRange
    )

  @Test def `range-literal` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: Int = {
         |    val l = List(1,2,3)
         |    l.map { x =>
         |      x.to(x*x)
         |        .flatMap { y =>
         |          List(y + <<%<%2137%>%>>)
         |        }
         |      .sum
         |    }.sum
         |  }
         |}
         |""".stripMargin,
      """|Int
         |def +(x: Int): Int""".stripMargin.hoverRange
    )

  @Test def `range-val-lhs-in-for` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: List[Int] =
         |    for {
         |      a <- List(11,23,17)
         |      b <- a to a*a
         |      <<%<%x%>% = b - 3>>
         |    } yield x
         |}
         |""".stripMargin,
      """|Int
         |val x: Int""".stripMargin.hoverRange
    )

  @Test def `range-binding-lhs-in-for` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: List[Int] =
         |    for {
         |      a <- List(11,23,17)
         |      <<%<%b%>%>> <- a to a*a
         |      x = b - 3
         |    } yield x
         |}
         |""".stripMargin,
      """|Int
         |b: Int""".stripMargin.hoverRange
    )

  @Test def `range-wider` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: Int = {
         |    val l = List(1,2,3)
         |   <<l.map { x =>
         |      x.to(x*x)
         |        .flatMap { y =>
         |          List(y + 2137)
         |        }
         |      .sum
         |    }.%<%  sum%>%>>
         |  }
         |}
         |""".stripMargin,
      """|Int
         |def sum[B >: Int](implicit num: Numeric[B]): B""".stripMargin.hoverRange
    )

  @Test def `range-wider2` =
    check(
      """|package helpers
         |
         |class XDClass {
         |  def xd: Int = {
         |    val l = List(1,2,3)
         |%<% <<l.map { x =>
         |      x.to(x*x)
         |        .flatMap { y =>
         |          List(y + 2137)
         |        }
         |      .sum
         |    }.sum>>  %>%
         |  }
         |}
         |""".stripMargin,
      """|Int
         |def sum[B >: Int](implicit num: Numeric[B]): B""".stripMargin.hoverRange
    )

  @Test def `transparent` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def foo(i: Int): Foo = new Bar
         |val bar = <<%<%foo(1)%>%>>
         |""".stripMargin,
      """|Bar
         |inline transparent def foo(i: Int): Foo""".stripMargin.hoverRange
    )

  @Test def `dep-types` =
    check(
      """|trait A
         |object A1 extends A
         |
         |trait Foo:
         |  type Out
         |  def out: Out
         |
         |def fooOut(f: Foo): f.Out = f.out
         |
         |object FooA1 extends Foo:
         |  type Out = A1.type
         |  def out = A1
         |
         |val x = <<%<%fooOut(FooA1)%>%>>
         |""".stripMargin,
      """|A1.type
         |def fooOut(f: Foo): f.Out""".stripMargin.hoverRange
    )
