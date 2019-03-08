package dotty.tools
package repl

import org.junit.Test
import org.junit.Assert.assertEquals

class DocTests extends ReplTest {

  @Test def docOfDef =
    eval("/** doc */ def foo = 0").andThen { implicit s =>
      assertEquals("doc", doc("foo"))
    }

  @Test def docOfVal =
    eval("/** doc */ val foo = 0").andThen { implicit s =>
      assertEquals("doc", doc("foo"))
    }

  @Test def docOfObject =
    eval("/** doc */ object Foo").andThen { implicit s =>
      assertEquals("doc", doc("Foo"))
    }

  @Test def docOfClass =
    eval("/** doc */ class Foo").andThen { implicit s =>
      assertEquals("doc", doc("new Foo"))
    }

  @Test def docOfTrait =
    eval("/** doc */ trait Foo").andThen { implicit s =>
      assertEquals("doc", doc("new Foo"))
    }

  @Test def docOfDefInObject =
    eval("object O { /** doc */ def foo = 0 }").andThen { implicit s =>
      assertEquals("doc", doc("O.foo"))
    }

  @Test def docOfValInObject =
    eval("object O { /** doc */ val foo = 0 }").andThen { implicit s =>
      assertEquals("doc", doc("O.foo"))
    }

  @Test def docOfObjectInObject =
    eval("object O { /** doc */ object Foo }").andThen { implicit s =>
      assertEquals("doc", doc("O.Foo"))
    }

  @Test def docOfClassInObject =
    eval("object O { /** doc */ class Foo }").andThen { implicit s =>
      assertEquals("doc", doc("new O.Foo"))
    }

  @Test def docOfTraitInObject =
    eval("object O { /** doc */ trait Foo }").andThen { implicit s =>
      assertEquals("doc", doc("new O.Foo"))
    }

  @Test def docOfDefInClass =
    eval(
      """class C { /** doc */ def foo = 0 }
        |val c = new C
      """.stripMargin).andThen { implicit s =>
      assertEquals("doc", doc("c.foo"))
    }

  @Test def docOfValInClass =
    eval(
      """class C { /** doc */ val foo = 0 }
        |val c = new C
      """.stripMargin).andThen { implicit s =>
      assertEquals("doc", doc("c.foo"))
    }

  @Test def docOfObjectInClass =
    eval(
      """class C { /** doc */ object Foo }
        |val c = new C
      """.stripMargin).andThen { implicit s =>
      assertEquals("doc", doc("c.Foo"))
    }

  @Test def docOfClassInClass =
    eval(
      """class C { /** doc */ class Foo }
        |val c = new C
      """.stripMargin).andThen { implicit s =>
      assertEquals("doc", doc("new c.Foo"))
    }

  @Test def docOfTraitInClass =
    eval(
      """class C { /** doc */ trait Foo }
        |val c = new C
      """.stripMargin).andThen { implicit s =>
      assertEquals("doc", doc("new c.Foo"))
    }

  @Test def docOfOverloadedDef =
    eval(
      """object O {
        |  /** doc0 */ def foo(x: Int) = x
        |  /** doc1 */ def foo(x: String) = x
        |}
      """.stripMargin).andThen { implicit s =>
      assertEquals("doc0", doc("O.foo(_: Int)"))
      assertEquals("doc1", doc("O.foo(_: String)"))
    }

  @Test def docOfInherited =
    eval(
      """class C { /** doc */ def foo = 0 }
        |object O extends C
      """.stripMargin).andThen { implicit s =>
      assertEquals("doc", doc("O.foo"))
    }

  @Test def docOfOverride =
    eval(
      """abstract class A {
        |  /** doc0 */ def foo(x: Int): Int = x + 1
        |  /** doc1 */ def foo(x: String): String = x + "foo"
        |}
        |object O extends A {
        |  override def foo(x: Int): Int = x
        |  /** overridden doc */ override def foo(x: String): String = x
        |}
      """.stripMargin).andThen { implicit s =>
      assertEquals("doc0", doc("O.foo(_: Int)"))
      assertEquals("overridden doc", doc("O.foo(_: String)"))
    }

  @Test def docOfOverrideObject =
    eval(
      """abstract class A {
        |  abstract class Companion { /** doc0 */ def bar: Int }
        |  /** companion */ def foo: Companion
        |}
        |object O extends A {
        |  override object foo extends Companion {
        |    override def bar: Int = 0
        |  }
        |}
      """.stripMargin).andThen { implicit s =>
      assertEquals("companion", doc("O.foo"))
      assertEquals("doc0", doc("O.foo.bar"))
    }

  @Test def docIsCooked =
    eval(
      """/**
        | * An object
        | * @define Variable some-value
        | */
        |object Foo {
        |  /** Expansion: $Variable */
        |  def hello = "world"
        |}
      """.stripMargin).andThen { implicit s =>
      assertEquals("Expansion: some-value", doc("Foo.hello"))
    }

  private def eval(code: String): State =
    fromInitialState { implicit s => run(code) }

  private def doc(expr: String)(implicit s: State): String = {
    storedOutput()
    run(s":doc $expr")
    storedOutput().trim
  }

}
