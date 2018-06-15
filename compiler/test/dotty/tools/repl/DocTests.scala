package dotty.tools
package repl

import org.junit.Test
import org.junit.Assert.assertEquals

class DocTests extends ReplTest {
  @Test def docOfDef =
    fromInitialState { implicit s => run("/** doc */ def foo = 0") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfVal =
    fromInitialState { implicit s => run("/** doc */ val foo = 0") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfObject =
    fromInitialState { implicit s => run("/** doc */ object Foo") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfClass =
    fromInitialState { implicit s => run("/** doc */ class Foo") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc new Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfTrait =
    fromInitialState { implicit s => run("/** doc */ trait Foo") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc new Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfDefInObject =
    fromInitialState { implicit s => run("object O { /** doc */ def foo = 0 }") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc O.foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfValInObject =
    fromInitialState { implicit s => run("object O { /** doc */ val foo = 0 }") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc O.foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfObjectInObject =
    fromInitialState { implicit s => run("object O { /** doc */ object Foo }") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc O.Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfClassInObject =
    fromInitialState { implicit s => run("object O { /** doc */ class Foo }") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc new O.Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfTraitInObject =
    fromInitialState { implicit s => run("object O { /** doc */ trait Foo }") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc new O.Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfDetInClass =
    fromInitialState { implicit s => run("class C { /** doc */ def foo = 0 }") }
    .andThen { implicit s => run("val c = new C") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc c.foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfVatInClass =
    fromInitialState { implicit s => run("class C { /** doc */ val foo = 0 }") }
    .andThen { implicit s => run("val c = new C") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc c.foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfObjectInClass =
    fromInitialState { implicit s => run("class C { /** doc */ object Foo }") }
    .andThen { implicit s => run("val c = new C") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc c.Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfClassInClass =
    fromInitialState { implicit s => run("class C { /** doc */ class Foo }") }
    .andThen { implicit s => run("val c = new C") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc new c.Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfTraitInClass =
    fromInitialState { implicit s => run("class C { /** doc */ trait Foo }") }
    .andThen { implicit s => run("val c = new C") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc new c.Foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

  @Test def docOfOverloadedDef =
    fromInitialState { implicit s =>
      run("""object O {
            |/** doc0 */ def foo(x: Int) = x
            |/** doc1 */ def foo(x: String) = x
            |}""".stripMargin)
    }
    .andThen { implicit s =>
      storedOutput()
      run(":doc O.foo(_: Int)")
      assertEquals("/** doc0 */", storedOutput().trim)
      s
    }
    .andThen { implicit s =>
      run(":doc O.foo(_: String)")
      assertEquals("/** doc1 */", storedOutput().trim)
    }

  @Test def docOfInherited =
    fromInitialState { implicit s => run("class C { /** doc */ def foo = 0 }") }
    .andThen { implicit s => run("object O extends C") }
    .andThen { implicit s =>
      storedOutput()
      run(":doc O.foo")
      assertEquals("/** doc */", storedOutput().trim)
    }

}
