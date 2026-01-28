package dotty.tools.pc.tests.signaturehelp

import dotty.tools.pc.base.BaseSignatureHelpSuite

import org.junit.Test

class SignatureHelpNamedArgsSuite extends BaseSignatureHelpSuite:

  @Test def `new-named-param-style-1` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, @@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-double-space` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1,  @@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-before-equal` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, paramB @@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-before-equal-something-after` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, paramB @@)
         |  def test: Unit = ()
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-at-equal` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, paramB =@@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-after-equal` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, paramB = 1@@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-2` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramB = 1, @@)
         |""".stripMargin,
      """|method([paramB: Int], [paramA: Int], [paramC: Int], [paramD: Int]): Unit
         |                      ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-3` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramB = 1, paramC = 2, paramD = 3, @@)
         |""".stripMargin,
      """|method([paramB: Int], [paramC: Int], [paramD: Int], [paramA: Int]): Unit
         |                                                    ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-4` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(1, 2, @@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                                 ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-5` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(1, paramB = 2, @@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                                 ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-6` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, 2, @@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                                 ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-7` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, paramB = 2, @@)
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                                 ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-8` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramB = 2, paramA = 1, @@)
         |""".stripMargin,
      """|method([paramB: Int], [paramA: Int], [paramC: Int], [paramD: Int]): Unit
         |                                     ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-9` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramC = 3, paramA = 1, @@)
         |""".stripMargin,
      """|method([paramC: Int], [paramA: Int], [paramB: Int], [paramD: Int]): Unit
         |                                     ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-10` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramC = 3, @@ ,paramA = 1)
         |""".stripMargin,
      """|method([paramC: Int], [paramB: Int], [paramA: Int], [paramD: Int]): Unit
         |                      ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-11` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramC = 3, paramB = 1, @@ ,paramA = 1)
         |""".stripMargin,
      """|method([paramC: Int], [paramB: Int], [paramD: Int], [paramA: Int]): Unit
         |                                     ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-param-before-first` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(@@paramC = 3)
         |""".stripMargin,
      """|method([paramC: Int], [paramA: Int], [paramB: Int], [paramD: Int]): Unit
         |       ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-param-inside-reordered-last-arg` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramB = 3, paramA = 1, @@paramD = 3, paramC = 1)
         |""".stripMargin,
      """|method([paramB: Int], [paramA: Int], [paramD: Int], [paramC: Int]): Unit
         |                                     ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-param-inside-reorderded-last-arg-1` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramB = 3, paramA = 1, p@@aramD = 3, paramC = 1)
         |""".stripMargin,
      """|method([paramB: Int], [paramA: Int], [paramD: Int], [paramC: Int]): Unit
         |                                     ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-param-before-reorderded-last-arg-1` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramB = 3, paramA = 1,@@ paramD = 3, paramC = 1)
         |""".stripMargin,
      """|method([paramB: Int], [paramA: Int], [paramD: Int], [paramC: Int]): Unit
         |                                     ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-param-properly-order-1` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(par@@amB = 3, paramA = 1, paramD = 3, paramC = 1)
         |""".stripMargin,
      """|method([paramB: Int], [paramA: Int], [paramD: Int], [paramC: Int]): Unit
         |       ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-param-properly-order-2` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramB = 3, par@@amA = 1, paramD = 3, paramC = 1)
         |""".stripMargin,
      """|method([paramB: Int], [paramA: Int], [paramD: Int], [paramC: Int]): Unit
         |                      ^^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-with-type-args` =
    check(
      """|object Main:
         |  def test[T, F](aaa: Int, bbb: T, ccc: F): T = ???
         |  val x = test(1, ccc = 2, b@@)
         |""".stripMargin,
      """|test[T, F](aaa: Int, [ccc: F], [bbb: T]): T
         |                               ^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-newline` =
    check(
      """|object Main:
         |  def test2(aaa: Int, bbb: Int, ccc: Int): Int = ???
         |  val x = test2(
         |      1,
         |      @@
         |    )
         |""".stripMargin,
      """|test2(aaa: Int, bbb: Int, ccc: Int): Int
         |                ^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-before-comma` =
    check(
      """|object Main:
         |  def test2(aaa: Int, bbb: Int, ccc: Int): Int = ???
         |  val x = test2(aaa = 2@@, ccc = 1)
         |""".stripMargin,
      """|test2(aaa: Int, [ccc: Int], [bbb: Int]): Int
         |      ^^^^^^^^
         |""".stripMargin
    )

  @Test def `named-on-whitespaces-between-args` =
    check(
      """|object Main:
         |  def test2(aaa: Int, bbb: Int, ccc: Int): Int = ???
         |  val x = test2(aaa = 2, @@  ccc = 1, bbb = 3)
         |
         |""".stripMargin,
      """|test2(aaa: Int, [ccc: Int], [bbb: Int]): Int
         |                ^^^^^^^^^^
         |""".stripMargin
    )
