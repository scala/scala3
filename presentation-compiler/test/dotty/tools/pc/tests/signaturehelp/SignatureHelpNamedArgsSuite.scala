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
         |  method(paramA = 1,  @@
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-before-equal` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, paramB @@
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-before-equal-something-after` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, paramB @@
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
         |  method(paramA = 1, paramB =@@
         |""".stripMargin,
      """|method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit
         |                    ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `new-named-param-style-after-equal` =
    check(
      """|object O:
         |  def method(paramA: Int, paramB: Int, paramC: Int, paramD: Int): Unit = ???
         |  method(paramA = 1, paramB = 1@@
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
      """|method([paramC: Int], [paramB: Int], [paramD: Int], [paramA: Int]): Unit
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
