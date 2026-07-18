package dotty.tools
package repl

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

class ReplProductToStringShapeTests extends ReplTest:

  private def assertRenderedAs(output: String, expected: String, structuralPrefix: String): Unit =
    assertTrue(output, output.contains(expected))
    assertFalse(output, output.contains(structuralPrefix))
    assertFalse(output, output.contains("-- Error:"))

  private def renderAfterDefinition(definition: String, expression: String)(using State): String =
    val afterDefinition = run(definition)
    storedOutput()
    run(expression)(using afterDefinition)
    storedOutput()

  @Test def `local case class with direct toString`: Unit =
    initially:
      val output = renderAfterDefinition(
        """|def makeLocalDirect: Product =
           |  case class LocalDirect(i: Int):
           |    override def toString: String = "local-direct(" + i + ")"
           |  LocalDirect(1)
           |""".stripMargin,
        "makeLocalDirect"
      )
      assertRenderedAs(output, "val res0: Product = local-direct(1)", "LocalDirect(1)")

  @Test def `local case class with inherited toString`: Unit =
    initially:
      val output = renderAfterDefinition(
        """|def makeLocalInherited: Product =
           |  trait LocalBase:
           |    override def toString: String = "local-inherited"
           |  case class LocalInherited(i: Int) extends LocalBase
           |  LocalInherited(2)
           |""".stripMargin,
        "makeLocalInherited"
      )
      assertRenderedAs(output, "val res0: Product = local-inherited", "LocalInherited(2)")

  @Test def `local case class without user toString remains structural`: Unit =
    initially:
      val output = renderAfterDefinition(
        """|def makeLocalPlain: Product =
           |  case class LocalPlain(i: Int, s: String)
           |  LocalPlain(3, "plain")
           |""".stripMargin,
        "makeLocalPlain"
      )
      assertRenderedAs(output, """val res0: Product = LocalPlain(i = 3, s = "plain")""", "LocalPlain(3,plain)")

  @Test def `block local case class with direct toString`: Unit =
    initially:
      run(
        """|val blockLocal: Product =
           |  case class BlockLocal(i: Int):
           |    override def toString: String = "block-local(" + i + ")"
           |  BlockLocal(4)
           |""".stripMargin
      )
      val output = storedOutput()
      assertRenderedAs(output, "val blockLocal: Product = block-local(4)", "BlockLocal(4)")

  @Test def `inner case class with direct toString`: Unit =
    initially:
      val afterClass = run(
        """|class OuterDirect:
           |  case class Inner(i: Int):
           |    override def toString: String = "inner-direct(" + i + ")"
           |""".stripMargin
      )
      storedOutput()

      val afterOuter = run("val outerDirect = new OuterDirect")(using afterClass)
      storedOutput()

      run("outerDirect.Inner(4)")(using afterOuter)
      val output = storedOutput()
      assertRenderedAs(output, " = inner-direct(4)", "Inner(4)")

  @Test def `inner case class without user toString remains structural`: Unit =
    initially:
      val afterClass = run(
        """|class OuterPlain:
           |  case class Inner(i: Int, s: String)
           |""".stripMargin
      )
      storedOutput()

      val afterOuter = run("val outerPlain = new OuterPlain")(using afterClass)
      storedOutput()

      run("""outerPlain.Inner(5, "inner")""")(using afterOuter)
      val output = storedOutput()
      assertRenderedAs(output, """Inner(i = 5, s = "inner")""", "Inner(5,inner)")

  @Test def `anonymous product with direct toString`: Unit =
    initially:
      run(
        """|new Product:
           |  def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
           |  def productArity: Int = 1
           |  def productElement(n: Int): Any =
           |    if n == 0 then 6 else throw new IndexOutOfBoundsException(n.toString)
           |  override def productPrefix: String = "AnonymousProduct"
           |  override def toString: String = "anonymous-product"
           |""".stripMargin
      )
      val output = storedOutput()
      assertRenderedAs(output, " = anonymous-product", "AnonymousProduct(6)")

  @Test def `anonymous product with inherited toString`: Unit =
    initially:
      val afterTrait = run(
        """|trait AnonymousBase:
           |  override def toString: String = "anonymous-inherited"
           |""".stripMargin
      )
      storedOutput()

      run(
        """|new Product with AnonymousBase:
           |  def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
           |  def productArity: Int = 1
           |  def productElement(n: Int): Any =
           |    if n == 0 then 7 else throw new IndexOutOfBoundsException(n.toString)
           |  override def productPrefix: String = "AnonymousInherited"
           |""".stripMargin
      )(using afterTrait)
      val output = storedOutput()
      assertRenderedAs(output, " = anonymous-inherited", "AnonymousInherited(7)")

  @Test def `anonymous product without user toString remains structural`: Unit =
    initially:
      run(
        """|new Product:
           |  def canEqual(that: Any): Boolean = that.isInstanceOf[Product]
           |  def productArity: Int = 1
           |  def productElement(n: Int): Any =
           |    if n == 0 then 8 else throw new IndexOutOfBoundsException(n.toString)
           |  override def productPrefix: String = "AnonymousPlain"
           |""".stripMargin
      )
      val output = storedOutput()
      assertRenderedAs(output, "AnonymousPlain(8)", "anonymous-plain")
