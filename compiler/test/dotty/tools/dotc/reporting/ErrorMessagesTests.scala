package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import diagnostic.messages._
import dotty.tools.dotc.core.Types.WildcardType
import dotty.tools.dotc.parsing.Tokens
import org.junit.Assert._
import org.junit.{Ignore, Test}

class ErrorMessagesTests extends ErrorMessagesTest {
  // In the case where there are no errors, we can do "expectNoErrors" in the
  // `Report`
  @Test def noErrors =
    checkMessagesAfter("frontend")("""class Foo""")
    .expectNoErrors

  @Test def typeMismatch =
    checkMessagesAfter("frontend") {
      """
      |object Foo {
      |  def bar: String = 1
      |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      // Assert that we only got one error message
      assertMessageCount(1, messages)

      // Pattern match out the expected error
      val TypeMismatch(found, expected, _, _) :: Nil = messages

      // The type of the right hand side will actually be the constant 1,
      // therefore we check if it "derivesFrom"  `IntClass`
      assert(found.derivesFrom(defn.IntClass), s"found was: $found")

      // The expected type is `scala.String` which we dealias to
      // `java.lang.String` and compare with `=:=` to `defn.StringType` which
      // is a type reference to `java.lang.String`
      assert(expected.dealias =:= defn.StringType, s"expected was: $expected")
    }

  @Test def overridesNothing =
    checkMessagesAfter("refchecks") {
      """
        |object Foo {
        |  override def bar: Unit = {}
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val OverridesNothing(member) :: Nil = messages
      assertEquals("bar", member.name.show)
    }

  @Test def overridesNothingDifferentSignature =
    checkMessagesAfter("refchecks") {
      """
        |class Bar {
        |  def bar(s: String): Unit = {}
        |  def bar(s: Int): Unit = {}
        |  final def bar(s: Long): Unit = {}
        |}
        |object Foo extends Bar {
        |  override def bar: Unit = {}
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val OverridesNothingButNameExists(member, sameName) :: Nil = messages
      // check expected context data
      assertEquals("bar", member.name.show)
      assertEquals(3, sameName.size)
      assert(sameName.forall(_.symbol.name.show == "bar"),
        "at least one method had an unexpected name")
    }

  @Test def forwardReference =
    checkMessagesAfter("refchecks") {
      """
        |object Forward {
        |  def block = {
        |    a.toInt
        |    val b = 2
        |    val a = BigDecimal("4")
        |  }
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        val defn = ictx.definitions

        assertMessageCount(1, messages)
        val ForwardReferenceExtendsOverDefinition(value, definition) :: Nil = messages
        assertEquals("value b", value.show)
        assertEquals("value a", definition.show)
      }

  @Test def unexpectedToken =
    checkMessagesAfter("frontend") {
      """
        |object Forward {
        |  def val = "ds"
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val ExpectedTokenButFound(expected, found) :: Nil = messages
      assertEquals(Tokens.IDENTIFIER, expected)
      assertEquals(Tokens.VAL, found)
    }

  @Test def expectedToken =
    checkMessagesAfter("frontend") {
      """
        |object Forward {
        |  def `val` = "ds"
        |}
      """.stripMargin
    }
    .expectNoErrors

  @Test def leftAndRightAssociative =
    checkMessagesAfter("frontend") {
      """
        |object Ops {
        |  case class I(j: Int) {
        |    def +-(i: Int) = i
        |    def +:(i: Int) = i
        |  }
        |  val v = I(1) +- I(4) +: I(4)
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val MixedLeftAndRightAssociativeOps(op1, op2, op2LeftAssoc) :: Nil = messages
      assertEquals("+-", op1.show)
      assertEquals("+:", op2.show)
      assertFalse(op2LeftAssoc)
    }

  @Test def cantInstantiateAbstract =
    checkMessagesAfter("refchecks") {
      """
        |object Scope {
        |  abstract class Concept
        |  new Concept()
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val CantInstantiateAbstractClassOrTrait(cls, isTrait) :: Nil = messages
      assertEquals("Concept", cls.name.show)
      assertFalse("expected class", isTrait)
    }

  @Test def cantInstantiateTrait =
    checkMessagesAfter("refchecks") {
      """
        |object Scope {
        |  trait Concept
        |  new Concept()
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val CantInstantiateAbstractClassOrTrait(cls, isTrait) :: Nil = messages
      assertEquals("Concept", cls.name.show)
      assertTrue("expected trait", isTrait)
    }

  @Test def constructorModifier =
    checkMessagesAfter("frontend") {
      """
        |class AnotherClass @deprecated ()
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val AnnotatedPrimaryConstructorRequiresModifierOrThis(cls) :: Nil = messages
      assertEquals("AnotherClass", cls.show)
    }

  @Test def overloadedMethodNeedsReturnType =
    checkMessagesAfter("frontend") {
      """
        |class Scope() {
        |  def foo(i: Int) = foo(i.toString)
        |  def foo(s: String) = s
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val OverloadedOrRecursiveMethodNeedsResultType(tree) :: Nil = messages
      assertEquals("foo", tree.show)
    }

  @Test def recursiveMethodNeedsReturnType =
    checkMessagesAfter("frontend") {
      """
        |class Scope() {
        |  def i = i + 5
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val OverloadedOrRecursiveMethodNeedsResultType(tree) :: Nil = messages
      assertEquals("i", tree.show)
    }

  @Test def recursiveValueNeedsReturnType =
    checkMessagesAfter("frontend") {
      """
        |class Scope() {
        |  lazy val i = i + 5
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val RecursiveValueNeedsResultType(tree) :: Nil = messages
      assertEquals("i", tree.show)
    }

  @Test def cyclicReferenceInvolving =
    checkMessagesAfter("frontend") {
      """
        |class A {
        |  val x: T = ???
        |  type T <: x.type // error: cyclic reference involving value x
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val CyclicReferenceInvolving(denot) :: Nil = messages
      assertEquals("value x", denot.show)
    }

  @Test def cyclicReferenceInvolvingImplicit =
    checkMessagesAfter("frontend") {
      """
        |object implicitDefs {
        |  def foo(implicit x: String) = 1
        |  def bar() = {
        |    implicit val x = foo
        |    x
        |  }
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val CyclicReferenceInvolvingImplicit(tree) :: Nil = messages
      assertEquals("x", tree.name.show)
    }

  @Test def superQualMustBeParent =
    checkMessagesAfter("frontend") {
      """
        |class A {
        |  def foo(): Unit = ()
        |}
        |
        |class B {
        |}
        |
        |class C extends A {
        |  super[B].foo
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        val defn = ictx.definitions

        assertMessageCount(1, messages)
        val SuperQualMustBeParent(qual, cls) :: Nil = messages

        assertEquals("B", qual.show)
        assertEquals("class C", cls.show)
      }

  @Test def ambiguousImport =
    checkMessagesAfter("frontend") {
      """
        |object A {
        |  class ToBeImported
        |}
        |object B {
        |  class ToBeImported
        |}
        |class C {
        |  import A.ToBeImported
        |  import B.ToBeImported
        |
        |  val value: ToBeImported = ???
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        val defn = ictx.definitions

        import typer.Typer.BindingPrec._

        assertMessageCount(1, messages)
        val AmbiguousImport(name, newPrec, prevPrec, prevCtx) :: Nil = messages
        assertEquals("ToBeImported", name.show)
        assertEquals(namedImport, newPrec)
        assertEquals(namedImport, prevPrec)
      }

  @Test def methodDoesNotTakePrameters =
    checkMessagesAfter("frontend") {
      """
        |object Scope{
        |  def foo = ()
        |  foo()
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val MethodDoesNotTakeParameters(tree, methodPart) :: Nil = messages

      assertEquals("Scope.foo", tree.show)
      assertEquals("=> Unit(Scope.foo)", methodPart.show)
    }

  @Test def methodDoesNotTakeMorePrameters =
    checkMessagesAfter("frontend") {
      """
        |object Scope{
        |  def foo(a: Int) = ()
        |  foo(1)("2")
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val MethodDoesNotTakeParameters(tree, methodPart) :: Nil = messages

      assertEquals("Scope.foo(1)", tree.show)
      assertEquals("((a: Int)Unit)(Scope.foo)", methodPart.show)
    }

  @Test def ambiugousOverloadWithWildcard =
    checkMessagesAfter("frontend") {
      """object Context {
        |  trait A {
        |    def foo(s: String): String
        |    def foo: String = foo("foo")
        |  }
        |  object B extends A {
        |    def foo(s: String): String = s
        |  }
        |  B.foo
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val AmbiguousOverload(tree, List(alt1, alt2), pt: WildcardType) :: Nil = messages
      assertEquals("method foo", alt1.show)
      assertEquals("(s: String)String", alt1.info.show)
      assertEquals("method foo", alt2.show)
    }

  @Test def reassignmentToVal =
    checkMessagesAfter("frontend") {
      """
        |class Context {
        |  val value = 3
        |  value = 4
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ReassignmentToVal(name) :: Nil = messages
      assertEquals("value", name.show)
    }

  @Test def typeDoesNotTakeParameters =
    checkMessagesAfter("frontend") {
      """
        |trait WithOutParams
        |class Extending extends WithOutParams[String]
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        val defn = ictx.definitions

        assertMessageCount(1, messages)
        val TypeDoesNotTakeParameters(tpe, params) :: Nil = messages
        assertEquals("WithOutParams", tpe.show)
      }

  @Test def parameterizedTypeLacksParameters =
    checkMessagesAfter("frontend") {
      """
        |trait WithParams(s: String)
        |class Extending extends WithParams
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        val defn = ictx.definitions

        assertMessageCount(1, messages)
        val ParameterizedTypeLacksArguments(symbol) :: Nil = messages
        assertEquals("trait WithParams", symbol.show)
      }

  @Test def varValParametersMayNotBeCallByName =
    checkMessagesAfter("frontend") {
      "trait Trait(val noNoNo: => String)"
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        assertMessageCount(1, messages)
        val VarValParametersMayNotBeCallByName(name, false) :: Nil = messages
        assertEquals("noNoNo", name.show)
      }

}
