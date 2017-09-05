package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import diagnostic.messages._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.FlagSet
import dotty.tools.dotc.core.Types.WildcardType
import dotty.tools.dotc.parsing.Tokens
import org.junit.Assert._
import org.junit.Test

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

  @Test def overloadedMethodNeedsReturnType = {
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
      val OverloadedOrRecursiveMethodNeedsResultType(treeName) :: Nil = messages
      assertEquals("foo", treeName)
    }


    checkMessagesAfter("frontend") {
      """
        |case class Foo[T](x: T)
        |object Foo { def apply[T]() = Foo(null.asInstanceOf[T]) }
      """.stripMargin
    }.expect { (ictx, messages) =>
      implicit val ctx: Context = ictx

      assertMessageCount(1, messages)
      val OverloadedOrRecursiveMethodNeedsResultType(treeName2) :: Nil = messages
      assertEquals("Foo", treeName2)
    }
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
      val OverloadedOrRecursiveMethodNeedsResultType(treeName) :: Nil = messages
      assertEquals("i", treeName)
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
        |object Scope {
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
      assertEquals("((a: Int): Unit)(Scope.foo)", methodPart.show)
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
      assertEquals("(s: String): String", alt1.info.show)
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

  @Test def missingTypeParameter =
    checkMessagesAfter("frontend") {
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
    checkMessagesAfter("refchecks") {
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
      assertEquals("scala.collection.immutable.List[Int]", bound.show)
    }

  @Test def doesNotConformToSelfType =
    checkMessagesAfter("refchecks") {
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
    checkMessagesAfter("refchecks") {
      """class Base
        |class RequiresBase { self: Base => }
        |object Scope {
        |  new RequiresBase
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
    checkMessagesAfter("frontend") {
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

  @Test def topLevelCantBeImplicit =
    checkMessagesAfter("frontend") {
      """package Foo {
        |  implicit object S
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val TopLevelCantBeImplicit(symbol) :: Nil = messages
      assertEquals("object S", symbol.show)
    }

  @Test def typesAndTraitsCantBeImplicit =
    checkMessagesAfter("frontend") {
      """class Foo {
        |  implicit trait S
        |}
        |""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val TypesAndTraitsCantBeImplicit(symbol) :: Nil = messages
      assertEquals("trait S", symbol.show)
    }

  @Test def onlyClassesCanBeAbstract =
    checkMessagesAfter("frontend") {
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
    checkMessagesAfter("frontend") {
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
    checkMessagesAfter("frontend") {
      """final trait Foo"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val TraitsMayNotBeFinal(symbol) :: Nil = messages
      assertEquals("trait Foo", symbol.show)
    }

  @Test def nativeMemberMayNotHaveImplementation =
    checkMessagesAfter("frontend") {
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
    checkMessagesAfter("frontend") {
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
    checkMessagesAfter("frontend") {
      """trait Foo extends AnyVal"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val CannotExtendAnyVal(symbol) :: Nil = messages
      assertEquals("trait Foo", symbol.show)
    }

  @Test def cannotHaveSameNameAs =
    checkMessagesAfter("refchecks") {
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
    checkMessagesAfter("refchecks") {
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
    checkMessagesAfter("refchecks") {
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
    checkMessagesAfter("refchecks") {
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
    checkMessagesAfter("refchecks") {
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
    checkMessagesAfter("refchecks") {
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
    checkMessagesAfter("refchecks") {
      """class MyValue(i: MyValue) extends AnyVal"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassesMayNotWrapItself(valueClass) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
    }

  @Test def valueClassParameterMayNotBeVar =
    checkMessagesAfter("refchecks") {
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
    checkMessagesAfter("refchecks") {
      """class MyValue() extends AnyVal"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val ValueClassNeedsOneValParam(valueClass) :: Nil = messages
      assertEquals("class MyValue", valueClass.show)
    }

  @Test def onlyCaseClassOrCaseObjectAllowed =
    checkMessagesAfter("frontend") {
      """case Foobar"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val err :: Nil = messages
      assertEquals(err, OnlyCaseClassOrCaseObjectAllowed())
    }

  @Test def expectedClassOrObjectDef =
    checkMessagesAfter("frontend") {
      """Foo"""
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      assertMessageCount(1, messages)
      val err :: Nil = messages
      assertEquals(err, ExpectedClassOrObjectDef())
    }

  @Test def anonymousFunctionMissingParamType =
    checkMessagesAfter("refchecks") {
      """
        |object AnonymousF {
        |  val f = { case l@List(1,2,3) => Some(l) }
        |}""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val AnonymousFunctionMissingParamType(param, args, _, pt) :: Nil = messages
      assertEquals("x$1", param.show)
      assertEquals(s"List(ValDef(${param.show},TypeTree,EmptyTree))", args.toString)
      assertEquals("?", pt.show)
    }

  @Test def superCallsNotAllowedInline =
  checkMessagesAfter("refchecks") {
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
      val SuperCallsNotAllowedInline(symbol) = err
      assertEquals("method bar", symbol.show)
    }

  @Test def modifiersNotAllowed =
    verifyModifiersNotAllowed("lazy trait T", "lazy", Some("trait"))

  @Test def modifiersOtherThanTraitMethodVariable =
    verifyModifiersNotAllowed("sealed lazy class x", "sealed")

  private def verifyModifiersNotAllowed(code: String, modifierAssertion: String,
                                        typeAssertion: Option[String] = None) = {
    checkMessagesAfter("refchecks")(code)
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        assertMessageCount(1, messages)
        val ModifiersNotAllowed(flags, sort) :: Nil = messages
        assertEquals(modifierAssertion, flags.toString)
        assertEquals(typeAssertion, sort)
      }
  }

  @Test def wildcardOnTypeArgumentNotAllowedOnNew =
    checkMessagesAfter("refchecks") {
      """
        |object TyperDemo {
        |  class Team[A]
        |  val team = new Team[_]
        |}""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val err :: Nil = messages

      assertEquals(err, WildcardOnTypeArgumentNotAllowedOnNew())
    }

  @Test def implicitFunctionTypeNeedsNonEmptyParameterList =
    checkMessagesAfter("refchecks") {
      """abstract class Foo {
        |  type Contextual[T] = implicit () => T
        |  val x: implicit () => Int
        |}""".stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(2, messages)
      messages.foreach(assertEquals(_, ImplicitFunctionTypeNeedsNonEmptyParameterList()))
    }

  @Test def wrongNumberOfParameters =
    checkMessagesAfter("refchecks") {
      """object NumberOfParams {
        |  def unary[T](x: T => Unit) = ()
        |  unary((x, y) => ())
        |} """.stripMargin
    }
    .expect { (ictx, messages) =>
      implicit val ctx: Context = ictx
      val defn = ictx.definitions

      assertMessageCount(1, messages)
      val err :: Nil = messages

      assertEquals(err, WrongNumberOfParameters(1))
    }

  @Test def duplicatePrivateProtectedQualifier =
    checkMessagesAfter("frontend") {
      """class Test {
        |   private[Test] protected[this] def foo(): Unit = ()
        |} """.stripMargin
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        val defn = ictx.definitions

        assertMessageCount(1, messages)
        val err :: Nil = messages

        assertEquals(DuplicatePrivateProtectedQualifier(), err)
      }

  @Test def expectedStartOfTopLevelDefinition =
    checkMessagesAfter("frontend") {
      """private Test {}"""
    }
      .expect { (ictx, messages) =>
        implicit val ctx: Context = ictx
        val defn = ictx.definitions

        assertMessageCount(1, messages)
        val err :: Nil = messages

        assertEquals(ExpectedStartOfTopLevelDefinition(), err)
      }
}
