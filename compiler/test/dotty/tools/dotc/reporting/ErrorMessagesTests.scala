package dotty.tools
package dotc
package reporting

import dotty.tools.backend.jvm.GenBCode
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.WildcardType
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.diagnostic.messages._
import dotty.tools.dotc.transform.{CheckStatic, PostTyper, TailRec}
import dotty.tools.dotc.typer.{FrontEnd, RefChecks}
import org.junit.Assert._
import org.junit.Test

class ErrorMessagesTests extends ErrorMessagesTest {
  // In the case where there are no errors, we can do "expectNoErrors" in the
  // `Report`
  @Test def noErrors =
    checkMessagesAfter(FrontEnd.name)("""class Foo""")
    .expectNoErrors

  @Test def caseClassExtendsEnum =
    checkMessagesAfter(RefChecks.name) {
      """
        |enum Foo { case A, B }
        |case class Bar() extends Foo
      """.stripMargin
    }
      .expect { (ictx, messages) ⇒
        implicit val ctx: Context = ictx
        assertMessageCount(1, messages)
        val errorMsg = messages.head
        val ClassCannotExtendEnum(cls, parent) :: Nil = messages
        assertEquals("Bar", cls.name.show)
        assertEquals("Foo", parent.name.show)
        assertEquals("<empty>", cls.owner.name.show)
      }

  @Test def typeMismatch =
    checkMessagesAfter(FrontEnd.name) {
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
    checkMessagesAfter(RefChecks.name) {
      """
        |object Foo {
        |  override def bar: Unit = {}
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val OverridesNothing(member) :: Nil = messages
      assertEquals("bar", member.name.show)
    }

  @Test def overridesNothingDifferentSignature =
    checkMessagesAfter(RefChecks.name) {
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

      assertMessageCount(1, messages)
      val OverridesNothingButNameExists(member, sameName) :: Nil = messages
      // check expected context data
      assertEquals("bar", member.name.show)
      assertEquals(3, sameName.size)
      assert(sameName.forall(_.symbol.name.show == "bar"),
        "at least one method had an unexpected name")
    }

  @Test def forwardReference =
    checkMessagesAfter(RefChecks.name) {
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

      assertMessageCount(1, messages)
      val ForwardReferenceExtendsOverDefinition(value, definition) :: Nil = messages
      assertEquals("value b", value.show)
      assertEquals("value a", definition.show)
    }

  @Test def unexpectedToken =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Forward {
        |  def val = "ds"
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val ExpectedTokenButFound(expected, found) :: Nil = messages
      assertEquals(Tokens.IDENTIFIER, expected)
      assertEquals(Tokens.VAL, found)
    }

  @Test def expectedToken =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Forward {
        |  def `val` = "ds"
        |}
      """.stripMargin
    }
    .expectNoErrors

  @Test def leftAndRightAssociative =
    checkMessagesAfter(FrontEnd.name) {
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

      assertMessageCount(1, messages)
      val MixedLeftAndRightAssociativeOps(op1, op2, op2LeftAssoc) :: Nil = messages
      assertEquals("+-", op1.show)
      assertEquals("+:", op2.show)
      assertFalse(op2LeftAssoc)
    }

  @Test def cantInstantiateAbstract =
    checkMessagesAfter(RefChecks.name) {
      """
        |object Scope {
        |  abstract class Concept
        |  val x = new Concept()
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val CantInstantiateAbstractClassOrTrait(cls, isTrait) :: Nil = messages
      assertEquals("Concept", cls.name.show)
      assertFalse("expected class", isTrait)
    }

  @Test def cantInstantiateTrait =
    checkMessagesAfter(RefChecks.name) {
      """
        |object Scope {
        |  trait Concept
        |  val x = new Concept()
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val CantInstantiateAbstractClassOrTrait(cls, isTrait) :: Nil = messages
      assertEquals("Concept", cls.name.show)
      assertTrue("expected trait", isTrait)
    }

  @Test def overloadedMethodNeedsReturnType =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class Scope() {
        |  def foo(i: Int) = foo(i.toString)
        |  def foo(s: String) = s
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val OverloadedOrRecursiveMethodNeedsResultType(cycleSym) :: Nil = messages
      assertEquals("foo", cycleSym.name.show)
    }

  @Test def i1731 =
    checkMessagesAfter(FrontEnd.name) {
      """
        |case class Foo[T](x: T)
        |object Foo { def apply[T]() = Foo(null.asInstanceOf[T]) }
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val OverloadedOrRecursiveMethodNeedsResultType(cycleSym) :: Nil = messages
        assertEquals("apply", cycleSym.name.show)
      }

  @Test def recursiveMethodNeedsReturnType =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class Scope() {
        |  def i = i + 5
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val OverloadedOrRecursiveMethodNeedsResultType(cycleSym) :: Nil = messages
      assertEquals("i", cycleSym.name.show)
    }

  @Test def recursiveValueNeedsReturnType =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class Scope() {
        |  lazy val i = i + 5
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val RecursiveValueNeedsResultType(cycleSym) :: Nil = messages
      assertEquals("i", cycleSym.name.show)
    }

  @Test def recursiveValueNeedsReturnType2 =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class Scope() {
        |  lazy val i = j + 5
        |  lazy val j = i
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val RecursiveValueNeedsResultType(cycleSym) :: Nil = messages
        assertEquals("i", cycleSym.name.show)
      }

  @Test def cyclicReferenceInvolving =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class A {
        |  val x: T = ???
        |  type T <: x.type // error: cyclic reference involving value x
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val CyclicReferenceInvolving(denot) :: Nil = messages
      assertEquals("value x", denot.show)
    }

  @Test def cyclicReferenceInvolving2 =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class A {
        |  implicit val x: T = ???
        |  type T <: x.type // error: cyclic reference involving value x
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val CyclicReferenceInvolving(denot) :: Nil = messages
        assertEquals("value x", denot.show)
      }

  @Test def mutualRecursionre_i2001 =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class A {
        |  def odd(x: Int) = if (x == 0) false else !even(x-1)
        |  def even(x: Int) = if (x == 0) true else !odd(x-1) // error: overloaded or recursive method needs result type
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val OverloadedOrRecursiveMethodNeedsResultType(cycleSym) :: Nil = messages
        assertEquals("odd", cycleSym.name.show)
      }

  @Test def mutualRecursion_i2001a =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class A {
        |  def odd(x: Int) = if (x == 0) false else !even(x-1)
        |  def even(x: Int) = {
        |    def foo = {
        |      if (x == 0) true else !odd(x-1) // error: overloaded or recursive method needs result type
        |    }
        |    false
        |  }
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val OverloadedOrRecursiveMethodNeedsResultType(cycleSym) :: Nil = messages
        assertEquals("odd", cycleSym.name.show)
      }

  @Test def mutualRecursion_i2001b =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class A {
        |  def odd(x: Int) = if (x == 0) false else !even(x-1)
        |  def even(x: Int) = {
        |    val foo = {
        |      if (x == 0) true else !odd(x-1) // error: overloaded or recursive method needs result type
        |    }
        |    false
        |  }
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val OverloadedOrRecursiveMethodNeedsResultType(cycleSym) :: Nil = messages
        assertEquals("odd", cycleSym.name.show)
      }

  @Test def termMemberNeedsResultTypeForImplicitSearch =
    checkMessagesAfter(FrontEnd.name) {
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

      assertMessageCount(1, messages)
      val TermMemberNeedsResultTypeForImplicitSearch(cycleSym) :: Nil = messages
      assertEquals("x", cycleSym.name.show)
    }

  @Test def implicitSearchForcesImplicitRetType_i4709 =
    checkMessagesAfter(FrontEnd.name) {
      """
        |import scala.language.implicitConversions
        |
        |class Context
        |class ContextBase { def settings = 1 }
        |
        |class Test {
        |  implicit def toBase(ctx: Context): ContextBase = ???
        |
        |  def test(ctx0: Context) = {
        |    implicit val ctx = { ctx0.settings; ??? }
        |  }
        |}
      """.stripMargin
    }
    .expect{ (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val TermMemberNeedsResultTypeForImplicitSearch(cycleSym) :: Nil = messages
      assertEquals("ctx", cycleSym.name.show)
    }

  @Test def implicitSearchForcesNonImplicitRetTypeOnExplicitImport_i3253 =
    checkMessagesAfter(FrontEnd.name) {
      """
        |import Test.test
        |
        |object Test {
        |  def test = "  " * 10
        |}
      """.stripMargin
    }
      .expect{ (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val TermMemberNeedsResultTypeForImplicitSearch(cycleSym) :: Nil = messages
        assertEquals("test", cycleSym.name.show)
      }

  @Test def superQualMustBeParent =
    checkMessagesAfter(FrontEnd.name) {
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

      assertMessageCount(1, messages)
      val SuperQualMustBeParent(qual, cls) :: Nil = messages

      assertEquals("B", qual.show)
      assertEquals("class C", cls.show)
    }

  @Test def ambiguousImport =
    checkMessagesAfter(FrontEnd.name) {
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

      import typer.Typer.BindingPrec._

      assertMessageCount(1, messages)
      val AmbiguousImport(name, newPrec, prevPrec, prevCtx) :: Nil = messages
      assertEquals("ToBeImported", name.show)
      assertEquals(namedImport, newPrec)
      assertEquals(namedImport, prevPrec)
    }

  @Test def methodDoesNotTakeParameters =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Scope {
        |  def foo = ()
        |  foo()
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val msg @ MethodDoesNotTakeParameters(tree) = messages.head

      assertEquals("Scope.foo", tree.show)
      assertEquals("method foo", msg.methodSymbol.show)
    }

  @Test def methodDoesNotTakeMoreParameters =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Scope{
        |  def foo(a: Int) = ()
        |  foo(1)("2")
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val msg @ MethodDoesNotTakeParameters(tree) = messages.head

      assertEquals("Scope.foo(1)", tree.show)
      assertEquals("method foo", msg.methodSymbol.show)
    }

  @Test def ambiugousOverloadWithWildcard =
    checkMessagesAfter(FrontEnd.name) {
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

      assertMessageCount(1, messages)
      val AmbiguousOverload(tree, List(alt1, alt2), pt: WildcardType) :: Nil = messages
      assertEquals("method foo", alt1.show)
      assertEquals("(s: String): String", alt1.info.show)
      assertEquals("method foo", alt2.show)
    }

  @Test def reassignmentToVal =
    checkMessagesAfter(FrontEnd.name) {
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
    checkMessagesAfter(FrontEnd.name) {
      """
        |trait WithOutParams
        |class Extending extends WithOutParams[String]
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val TypeDoesNotTakeParameters(tpe, params) :: Nil = messages
      assertEquals("WithOutParams", tpe.show)
    }

  @Test def parameterizedTypeLacksParameters =
    checkMessagesAfter(FrontEnd.name) {
      """
        |trait WithParams(s: String)
        |class Extending extends WithParams
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val ParameterizedTypeLacksArguments(symbol) :: Nil = messages
      assertEquals("trait WithParams", symbol.show)
    }

  @Test def varValParametersMayNotBeCallByName =
    checkMessagesAfter(FrontEnd.name) {
      "trait Trait(val noNoNo: => String)"
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val VarValParametersMayNotBeCallByName(name, false) :: Nil = messages
      assertEquals("noNoNo", name.show)
    }

  @Test def missingTypeParameter =
    checkMessagesAfter(FrontEnd.name) {
      """object Scope {
        |  val value: List = null
        |}""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val MissingTypeParameterFor(tpe) :: Nil = messages
      assertEquals("List", tpe.show)
    }

  @Test def doesNotConformToBound =
    checkMessagesAfter(RefChecks.name) {
      """class WithParam[A <: List[Int]]
        |object Scope {
        |  val value: WithParam[Int] = null
        |}""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val DoesNotConformToBound(tpe, which, bound) :: Nil = messages
      assertEquals("Int", tpe.show)
      assertEquals("upper", which)
      assertEquals("List[Int]", bound.show)
    }

  @Test def doesNotConformToSelfType =
    checkMessagesAfter(RefChecks.name) {
      """class Base
        |trait BlendItIn {
        |  this: Base =>
        |}
        |class Blended extends BlendItIn
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val DoesNotConformToSelfType(category, selfType, cls, otherSelf, relation, other) :: Nil = messages
      assertEquals("illegal inheritance", category)
      assertEquals("Blended", selfType.show)
      assertEquals("class Blended", cls.show)
      assertEquals("Base", otherSelf.show)
      assertEquals("parent", relation)
      assertEquals("trait BlendItIn", other.show)
    }

  @Test def doesNotConformToSelfTypeCantBeInstantiated =
    checkMessagesAfter(RefChecks.name) {
      """class Base
        |class RequiresBase { self: Base => }
        |object Scope {
        |  val x = new RequiresBase
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val DoesNotConformToSelfTypeCantBeInstantiated(tpe, selfType) :: Nil = messages
      assertEquals("RequiresBase", tpe.show)
      assertEquals("Base", selfType.show)
    }

  @Test def abstractValueMayNotHaveFinalModifier =
    checkMessagesAfter(FrontEnd.name) {
      """abstract class Foo {
        |  final val s: String
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val AbstractMemberMayNotHaveModifier(symbol, flags) :: Nil = messages
      assertEquals("value s", symbol.show)
      assertEquals("final", flags.toString)
    }

  @Test def typesAndTraitsCantBeImplicit =
    checkMessagesAfter(FrontEnd.name) {
      """class Foo {
        |  implicit trait S
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val TypesAndTraitsCantBeImplicit() :: Nil = messages
    }

  @Test def onlyClassesCanBeAbstract =
    checkMessagesAfter(FrontEnd.name) {
      """class Foo {
        |  abstract val s: String
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val OnlyClassesCanBeAbstract(symbol) :: Nil = messages
      assertEquals("value s", symbol.show)
    }

  @Test def abstractOverrideOnlyInTraits =
    checkMessagesAfter(FrontEnd.name) {
      """class Foo {
        |  abstract override val s: String = ""
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val AbstractOverrideOnlyInTraits(symbol) :: Nil = messages
      assertEquals("value s", symbol.show)
    }

  @Test def traitMayNotBeFinal =
    checkMessagesAfter(FrontEnd.name) {
      """final trait Foo"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val TraitsMayNotBeFinal(symbol) :: Nil = messages
      assertEquals("trait Foo", symbol.show)
    }

  @Test def nativeMemberMayNotHaveImplementation =
    checkMessagesAfter(FrontEnd.name) {
      """trait Foo {
        |  @native def foo() = 5
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val NativeMembersMayNotHaveImplementation(symbol) :: Nil = messages
      assertEquals("method foo", symbol.show)
    }

  @Test def onlyClassesCanHaveDeclaredButUndefinedMembers =
    checkMessagesAfter(FrontEnd.name) {
      """object Foo {
        |  def foo(): Int
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val OnlyClassesCanHaveDeclaredButUndefinedMembers(symbol) :: Nil = messages
      assertEquals("method foo", symbol.show)
    }

  @Test def cannotExtendAnyval =
    checkMessagesAfter(FrontEnd.name) {
      """trait Foo extends AnyVal"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val CannotExtendAnyVal(symbol) :: Nil = messages
      assertEquals("trait Foo", symbol.show)
    }

  @Test def cannotHaveSameNameAs =
    checkMessagesAfter(RefChecks.name) {
      """trait Foo {
        |  class A
        |}
        |class B extends Foo {
        |  class A
        |}""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val CannotHaveSameNameAs(symbol, cls, _) :: Nil = messages
      assertEquals("class A", symbol.show)
      assertEquals("class A", cls.show)
    }

  @Test def valueClassesMayNotDefineInner =
    checkMessagesAfter(RefChecks.name) {
      """class MyValue(i: Int) extends AnyVal {
        |  class Inner
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassesMayNotDefineInner(valueClass, inner) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
      assertEquals("class Inner", inner.show)
    }

  @Test def valueClassesMayNotDefineNonParameterField =
    checkMessagesAfter(RefChecks.name) {
      """class MyValue(i: Int) extends AnyVal {
        |  val illegal: Int
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassesMayNotDefineNonParameterField(valueClass, field) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
      assertEquals("value illegal", field.show)
    }

  @Test def valueClassesMayNotDefineASecondaryConstructor =
    checkMessagesAfter(RefChecks.name) {
      """class MyValue(i: Int) extends AnyVal {
        |  def this() = this(2)
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassesMayNotDefineASecondaryConstructor(valueClass, constuctor) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
      assertEquals("constructor MyValue", constuctor.show)
    }

  @Test def valueClassesMayNotContainInitalization =
    checkMessagesAfter(RefChecks.name) {
      """class MyValue(i: Int) extends AnyVal {
        |  println("Hallo?")
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassesMayNotContainInitalization(valueClass) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
    }

  @Test def valueClassesMayNotBeContained =
    checkMessagesAfter(RefChecks.name) {
      """class Outer {
        |  class MyValue(i: Int) extends AnyVal
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassesMayNotBeContainted(valueClass) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
    }

  @Test def valueClassesMayNotWrapItself =
    checkMessagesAfter(RefChecks.name) {
      """class MyValue(i: MyValue) extends AnyVal"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassesMayNotWrapItself(valueClass) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
    }

  @Test def valueClassParameterMayNotBeVar =
    checkMessagesAfter(RefChecks.name) {
      """class MyValue(var i: Int) extends AnyVal"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassParameterMayNotBeAVar(valueClass, param) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
      assertEquals("variable i", param.show)
    }

  @Test def valueClassNeedsOneVal =
    checkMessagesAfter(RefChecks.name) {
      """class MyValue() extends AnyVal"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassNeedsOneValParam(valueClass) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
    }

  @Test def valueClassParameterMayNotBeCallByName =
    checkMessagesAfter(RefChecks.name) {
      """class MyValue(a: => Int) extends AnyVal"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassParameterMayNotBeCallByName(valueClass, param) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
      assertEquals("value a", param.show)
    }

  @Test def onlyCaseClassOrCaseObjectAllowed =
    checkMessagesAfter(FrontEnd.name) {
      """case Foobar"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val err :: Nil = messages
      assertEquals(err, OnlyCaseClassOrCaseObjectAllowed())
    }

  @Test def expectedClassOrObjectDef =
    checkMessagesAfter(FrontEnd.name) {
      """Foo"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val err :: Nil = messages
      assertEquals(err, ExpectedToplevelDef())
    }

  @Test def implicitClassPrimaryConstructorArity =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Test {
        |  implicit class Foo(i: Int, s: String)
        |}
      """.stripMargin
    }
    .expect { (itcx, messages) =>
      implicit val ctx: Context = itcx
      assertMessageCount(1, messages)
      val err :: Nil = messages
      assertEquals(err, ImplicitClassPrimaryConstructorArity())
    }

  @Test def anonymousFunctionMissingParamType =
    checkMessagesAfter(RefChecks.name) {
      """
        |object AnonymousF {
        |  val f = { case x: Int => x + 1 }
        |}""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val AnonymousFunctionMissingParamType(param, args, _, pt) = messages.head
      assertEquals("x$1", param.show)
      assertEquals("?", pt.show)
    }

  @Test def superCallsNotAllowedInline =
  checkMessagesAfter(RefChecks.name) {
       """
        |class A {
        |  def foo(): Unit = ()
        |}
        |
        |class B extends A {
        |  inline def bar(): Unit = super.foo()
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val err :: Nil = messages
      val SuperCallsNotAllowedInlineable(symbol) = err
      assertEquals("method bar", symbol.show)
    }

  @Test def modifiersNotAllowed =
    verifyModifiersNotAllowed("lazy trait T", "lazy", Some("trait"))

  @Test def modifiersOtherThanTraitMethodVariable =
    verifyModifiersNotAllowed("sealed lazy class x", "sealed")

  private def verifyModifiersNotAllowed(code: String, modifierAssertion: String,
                                        typeAssertion: Option[String] = None) = {
    checkMessagesAfter(RefChecks.name)(code)
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        assertMessageCount(1, messages)
        val ModifiersNotAllowed(flags, sort) :: Nil = messages
        assertEquals(modifierAssertion, flags.toString)
        assertEquals(typeAssertion, sort)
      }
  }

  @Test def wildcardOnTypeArgumentNotAllowedOnNew =
    checkMessagesAfter(RefChecks.name) {
      """
        |object TyperDemo {
        |  class Team[A]
        |  val team = new Team[_]
        |}""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val err :: Nil = messages

      assertEquals(err, WildcardOnTypeArgumentNotAllowedOnNew())
    }

  @Test def wrongNumberOfParameters =
    checkMessagesAfter(RefChecks.name) {
      """object NumberOfParams {
        |  def unary[T](x: T => Unit) = ()
        |  unary((x, y) => ())
        |} """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val err :: Nil = messages

      assertEquals(err, WrongNumberOfParameters(1))
    }

  @Test def duplicatePrivateProtectedQualifier =
    checkMessagesAfter(FrontEnd.name) {
      """class Test {
        |   private[Test] protected[this] def foo(): Unit = ()
        |} """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val err :: Nil = messages

        assertEquals(DuplicatePrivateProtectedQualifier(), err)
      }

  @Test def expectedStartOfTopLevelDefinition =
    checkMessagesAfter(FrontEnd.name) {
      """private Test {}"""
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val err :: Nil = messages

        assertEquals(ExpectedStartOfTopLevelDefinition(), err)
      }

  @Test def missingReturnTypeWithReturnStatement =
    checkMessagesAfter(FrontEnd.name) {
      """class BadFunction {
        |  def bad() = { return "fail" }
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)

      val MissingReturnTypeWithReturnStatement(method) :: Nil = messages
      assertEquals(method.name.show, "bad")
    }

  @Test def noReturnInInline =
    checkMessagesAfter(FrontEnd.name) {
      """class BadFunction {
        |  inline def usesReturn: Int = { return 42 }
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)

      val NoReturnFromInlineable(method) :: Nil = messages
      assertEquals("method usesReturn", method.show)
    }

  @Test def returnOutsideMethodDefinition =
    checkMessagesAfter(FrontEnd.name) {
      """object A {
        |  return 5
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ReturnOutsideMethodDefinition(owner) :: Nil = messages
      assertEquals("object A", owner.show)
    }

  @Test def extendFinalClass = checkMessagesAfter(RefChecks.name) {
    """final class A
      |
      |class B extends A
    """.stripMargin
  }.expect { (ictx, messages) =>
    implicit val ctx: Context = ictx
    assertMessageCount(1, messages)
    val ExtendFinalClass(extender, parent) :: Nil = messages
    assertEquals(extender.show, "class B")
    assertEquals(parent.show, "class A")
  }

  @Test def tailrecNotApplicableNeitherPrivateNorFinal =
    checkMessagesAfter(TailRec.name) {
    """
      |class Foo {
      |  @scala.annotation.tailrec
      |  def foo: Unit = foo
      |}
    """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val TailrecNotApplicable(method) :: Nil = messages
      assertEquals(method.show, "method foo")
    }

    @Test def expectedTypeBoundOrEquals =
      checkMessagesAfter(FrontEnd.name) {
        """object typedef {
          |  type asd > Seq
          |}
        """.stripMargin
      }.expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(1, messages)
        val ExpectedTypeBoundOrEquals(found) :: Nil = messages
        assertEquals(Tokens.IDENTIFIER, found)
      }

  @Test def classAndCompanionNameClash =
    checkMessagesAfter(RefChecks.name) {
      """
        |class T {
        |  class G
        |}
        |object T {
        |  trait G
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val ClassAndCompanionNameClash(cls, other) :: Nil = messages

      assertEquals("class T", cls.owner.show)
      assertEquals("class G", cls.show)
      assertEquals("object T", other.owner.show)
      assertEquals("trait G", other.show)

    }

  @Test def onlyFunctionsCanBeFollowedByUnderscore =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class T {
        |  def main(args: Array[String]): Unit = {
        |   val n = "T"
        |   val func = n _
        |  }
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val OnlyFunctionsCanBeFollowedByUnderscore(tp) :: Nil = messages
      assertEquals("String", tp.show)
    }

  @Test def missingEmptyArgumentList =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class Test {
        |  def greet(): String = "Hello"
        |  def main(args: Array[String]): Unit = {
        |    greet
        |  }
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val MissingEmptyArgumentList(method) :: Nil = messages
      assertEquals("method greet", method.show)
    }

  @Test def duplicateNamedTypeParameter =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Test {
        |  def f[A, B]() = ???
        |  f[A=Any, A=Any]()
        |  f[B=Any, B=Any]()
        |}
        |
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(2, messages)
      val DuplicateNamedTypeParameter(n2) :: DuplicateNamedTypeParameter(n1) :: Nil = messages
      assertEquals("A", n1.show)
      assertEquals("B", n2.show)
    }

  @Test def undefinedNamedTypeParameter =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Test {
        |  def f[A, B]() = ???
        |  f[A=Any, C=Any]()
        |  f[C=Any, B=Any]()
        |}
        |
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(2, messages)
        val UndefinedNamedTypeParameter(n2, l2) :: UndefinedNamedTypeParameter(n1, l1) :: Nil = messages
        val tpParams = List("A", "B")
        assertEquals("C", n1.show)
        assertEquals(tpParams, l1.map(_.show))
        assertEquals("C", n2.show)
        assertEquals(tpParams, l2.map(_.show))

      }

  @Test def illegalStartOfStatement =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Test {
        |  { ) }
        |  { private ) }
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx

        assertMessageCount(2, messages)
        val errWithModifier :: err :: Nil = messages

        assertEquals(IllegalStartOfStatement(isModifier = false), err)
        assertEquals(IllegalStartOfStatement(isModifier = true), errWithModifier)
      }

  @Test def traitIsExpected =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class A
        |class B
        |
        |object Test {
        |  def main(args: Array[String]): Unit = {
        |    val a = new A with B
        |  }
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val TraitIsExpected(symbol) :: Nil = messages
      assertEquals("class B", symbol.show)
    }

  @Test def traitRedefinedFinalMethodFromAnyRef =
    checkMessagesAfter(RefChecks.name) {
      """
        |trait C {
        |  def wait (): Unit
        |}
      """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val TraitRedefinedFinalMethodFromAnyRef(method) = messages.head
      assertEquals("method wait", method.show)
    }

  @Test def packageNameAlreadyDefined =
    checkMessagesAfter(FrontEnd.name) {
      """
        |package bar { }
        |object bar { }
        |
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      val PackageNameAlreadyDefined(pkg) = messages.head
      assertEquals(pkg.show, "object bar")
    }

  @Test def unapplyInvalidNumberOfArguments =
    checkMessagesAfter(FrontEnd.name) {
      """
        |case class Boo(a: Int, b: String)
        |
        |object autoTuplingNeg2 {
        |  val z = Boo(1, "foo")
        |
        |  z match {
        |    case Boo(a, b, c) => a
        |  }
        |}
      """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        assertMessageCount(1, messages)
        val UnapplyInvalidNumberOfArguments(qual, argTypes) :: Nil = messages
        assertEquals("Boo", qual.show)
        assertEquals("(class Int, type String)", argTypes.map(_.typeSymbol).mkString("(", ", ", ")"))
      }

  @Test def unapplyInvalidReturnType =
    checkMessagesAfter("frontend") {
      """
        |class A(val i: Int)
        |
        |object A {
        |  def unapply(a: A): Int = a.i
        |  def test(a: A) = a match {
        |    case A() => 1
        |  }
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val UnapplyInvalidReturnType(unapplyResult, unapplyName) :: Nil = messages
      assertEquals("Int", unapplyResult.show)
      assertEquals("unapply", unapplyName.show)
    }

  @Test def unapplySeqInvalidReturnType =
    checkMessagesAfter("frontend") {
      """
        |class A(val i: Int)
        |
        |object A {
        |  def unapplySeq(a: A): Int = a.i
        |  def test(a: A) = a match {
        |    case A() => 1
        |  }
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val UnapplyInvalidReturnType(unapplyResult, unapplyName) :: Nil = messages
      assertEquals("Int", unapplyResult.show)
      assertEquals("unapplySeq", unapplyName.show)
    }

  @Test def staticOnlyAllowedInsideObjects =
    checkMessagesAfter(CheckStatic.name) {
      """
        |class Foo {
        |  @annotation.static def bar(): Unit = bar()
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val StaticFieldsOnlyAllowedInObjects(field) = messages.head
      assertEquals(field.show, "method bar")
    }

  @Test def staticShouldPrecedeNonStatic =
    checkMessagesAfter(CheckStatic.name) {
      """
        |class Foo
        |object Foo {
        |  val foo = 1
        |  @annotation.static val bar = 2
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val StaticFieldsShouldPrecedeNonStatic(field, _) = messages.head
      assertEquals(field.show, "value bar")
    }

  @Test def cyclicInheritance =
    checkMessagesAfter(FrontEnd.name) {
      "class A extends A"
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val CyclicInheritance(symbol, _) :: Nil = messages
      assertEquals("class A", symbol.show)
    }

  @Test def missingCompanionForStatic =
    checkMessagesAfter(CheckStatic.name) {
      """
        |object Foo {
        |  @annotation.static def bar(): Unit = ()
        |}
      """.stripMargin
    }.expect { (itcx, messages) =>
      implicit val ctx: Context = itcx
      val MissingCompanionForStatic(member) = messages.head
      assertEquals(member.show, "method bar")
    }

  @Test def polymorphicMethodMissingTypeInParent =
    checkMessagesAfter(FrontEnd.name) {
      """
        |object Test {
        |  import scala.reflect.Selectable.reflectiveSelectable
        |  def foo(x: { def get[T](a: T): Int }) = 5
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val PolymorphicMethodMissingTypeInParent(rsym, parentSym) = messages.head
      assertEquals("method get", rsym.show)
      assertEquals("class Object", parentSym.show)
    }

  @Test def javaSymbolIsNotAValue =
    checkMessagesAfter(CheckStatic.name) {
      """
        |package p
        |object O {
        |  val v = p
        |}
      """.stripMargin
    }.expect { (itcx, messages) =>
      implicit val ctx: Context = itcx

      assertMessageCount(1, messages)
      val JavaSymbolIsNotAValue(symbol) = messages.head
      assertEquals(symbol.show, "package p")
    }

  @Test def i3187 =
    checkMessagesAfter(GenBCode.name) {
      """
        |package scala
        |object collection
      """.stripMargin
    }.expect { (itcx, messages) =>
      implicit val ctx: Context = itcx

      assert(ctx.reporter.hasErrors)
    }

  @Test def typeDoubleDeclaration =
    checkMessagesAfter(FrontEnd.name) {
      """
        |class Foo {
        |  val a = 1
        |  val a = 2
        |}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val DoubleDefinition(symbol, previousSymbol, _) :: Nil = messages
      assertEquals(symbol.name.mangledString, "a")
  }

  @Test def renameImportTwice =
    checkMessagesAfter(PostTyper.name) {
      """
        |import java.lang.{Integer => Foo, Integer => Baz}
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val (msg @ ImportRenamedTwice(ident)) :: Nil = messages
      assertEquals(ident.show, "Integer")
    }

  @Test def tailRecOptimisation =
    checkMessagesAfter(FrontEnd.name) {
      """
        |import scala.annotation.tailrec
        |@tailrec
        |object Test {
        |  @tailrec val a = ""
        |  @tailrec var b = ""
        |}
        |@tailrec
        |class Test {}
        |
      """.stripMargin
    }.expect{ (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(4, messages)

      val tailRecMessages = messages.map({ case TailrecNotApplicable(sym) => sym.showKind }).toSet
      assertEquals(tailRecMessages, Set("variable", "value", "object", "class"))
    }

  @Test def notAnExtractor() =
    checkMessagesAfter(FrontEnd.name) {
      """
        | trait Foo
        | object Foo
        | object Test {
        |   def test(foo: Foo) = foo match {
        |       case Foo(name) => ???
        |   }
        | }
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val NotAnExtractor(tree) = messages.head
      assertEquals("Foo", tree.show)
    }

  @Test def memberWithSameNameAsStatic() =
    checkMessagesAfter(CheckStatic.name) {
      """
        |import scala.annotation.static
        |class Camp {
        |  val name = ""
        |}
        |object Camp {
        |  @static val name = ""
        |}
      """.stripMargin
    }.expect { (_, messages) =>
      assertMessageCount(1, messages)
      val message = messages.head
      assertTrue(message.isInstanceOf[MemberWithSameNameAsStatic])
      assertEquals(message.msg, "Companion classes cannot define members with same name as a @static member")
    }

  @Test def companionOfTraitWithMutableStatic() =
    checkMessagesAfter(CheckStatic.name) {
      """
        | import scala.annotation.static
        | trait Test
        | object Test {
        |   @static var myStatic = ""
        | }
      """.stripMargin
    }.expect { (_, messages) =>
      assertMessageCount(1, messages)
      val message = messages.head
      assertTrue(message.isInstanceOf[TraitCompanionWithMutableStatic])
      assertEquals(
        "Companion of traits cannot define mutable @static fields",
        message.msg
      )
    }

  @Test def lazyStaticField() =
    checkMessagesAfter(CheckStatic.name) {
      """
        | import scala.annotation.static
        | class Test
        | object Test {
        |   @static lazy val myStatic = ""
        | }
      """.stripMargin
    }.expect { (_, messages) =>
      assertMessageCount(1, messages)
      val message = messages.head
      assertTrue(message.isInstanceOf[LazyStaticField])
      assertEquals(
        "Lazy @static fields are not supported",
        message.msg
      )
    }

  @Test def staticOverridingNonStatic() =
    checkMessagesAfter(CheckStatic.name) {
      """
        | import scala.annotation.static
        | trait Foo {
        |   val foo = ""
        | }
        | class Test
        | object Test extends Foo {
        |   @static val foo = ""
        | }
      """.stripMargin
    }.expect { (_, messages) =>
      assertMessageCount(1, messages)
      val message = messages.head
      assertTrue(message.isInstanceOf[StaticOverridingNonStaticMembers])
      assertEquals(
        "@static members cannot override or implement non-static ones",
        message.msg
      )
    }

    @Test def StableIdentifiers() =
      checkMessagesAfter(FrontEnd.name) {
        """
          | object Test {
          |   var x = 2
          |   def test = 2 match {
          |     case `x` => x + 1
          |   }
          | }
        """.stripMargin
      }.expect { (_, messages) =>
        assertMessageCount(1, messages)
        val message = messages.head
        assertTrue(message.isInstanceOf[StableIdentPattern])
        assertEquals(
          "Stable identifier required, but `x` found",
          message.msg
        )
      }
}
