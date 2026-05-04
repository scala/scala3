package dotty.tools
package repl

import org.junit.Assert.*
import org.junit.Test

class TypeTests extends ReplTest:
  @Test def typeOf1 = initially {
    run(":type 1")
    assertEquals("Int", storedOutput().trim)
  }

  @Test def typeOfBlock = initially {
    run(":type { /** omg omg omg */ 1 + 5; 1 }")
    assertEquals("Int", storedOutput().trim)
  }

  @Test def typeOfX =
    initially {
      run("val x = 5")
    } andThen {
      storedOutput() // discard output
      run(":type x")
      assertEquals("Int", storedOutput().trim)
    }

  @Test def typeOfEmpty = initially {
    run(":type")
    assertEquals(":type <expression>", storedOutput().trim)
  }

  // scala/scala3#25465: :type on a type (not a term) should give a proper error, not crash
  @Test def `i25465 type command with CC enabled` =
    initially {
      run("import language.experimental.captureChecking")
    } andThen {
      storedOutput() // discard
      run("trait Foo")
    } andThen {
      storedOutput() // discard
      run(":type Foo")
      val output = storedOutput().trim
      assertTrue(s"Expected error about term/type mismatch, got: $output",
        output.contains("Not Found Error") || output.contains("Expected a term"))
    }

  // scala/scala3#25465: :type with ill-formed expression should give proper error with CC
  @Test def `i25465 type command ill-formed expr with CC` =
    initially {
      run("import language.experimental.captureChecking")
    } andThen {
      storedOutput() // discard
      run(":type def foo[D](x: D): C = x")
      val output = storedOutput().trim
      // Should produce an error, not crash
      assertTrue(s"Expected an error message, got: $output", output.nonEmpty)
    }

  // scala/scala3#25465: :type should show capture annotations when CC is enabled
  @Test def `i25465 type command shows captures` =
    initially {
      run("import language.experimental.captureChecking")
    } andThen {
      storedOutput() // discard
      run("val x = new Object")
    } andThen {
      storedOutput() // discard
      run(":type x")
      val output = storedOutput().trim
      assertTrue(s"Expected a type, got: $output",
        output.nonEmpty && !output.contains("error"))
    }

  // Type inference for capabilities in the REPL should work without
  // explicit type annotations, even though vals are wrapped in objects.
  @Test def `cc type inference for capabilities` =
    initially {
      run("import language.experimental.captureChecking")
      run("import caps.*")
    } andThen {
      storedOutput()
      run("class File extends SharedCapability")
    } andThen {
      assertEquals("// defined class File", storedOutput().trim)
      run(":type File()")
    } andThen {
      assertEquals("File^", storedOutput().trim)
      run("val x = File()")
    } andThen {
      assertTrue(storedOutput().trim.startsWith("val x: File^"))
      run("class Logger(f: File^) extends SharedCapability")
    } andThen {
      assertEquals("// defined class Logger", storedOutput().trim)
      run("val l = Logger(x)")
    } andThen {
      val lOut = storedOutput().trim
      assertTrue(s"expected Logger with captures, got: $lOut",
        lOut.contains("Logger") && lOut.contains("{") && lOut.contains("x"))
      run(":type l")
    } andThen {
      assertEquals("Logger{val f: File^{x}}^{l}", storedOutput().trim)
      run(":type Logger(x)")
      assertEquals("Logger{val f: File^{x}}^{any, x}", storedOutput().trim)
    }

  // :type should display inferred capture sets on function types
  @Test def `cc type command shows capture set on function` =
    initially {
      run("import language.experimental.captureChecking")
    } andThen {
      storedOutput() // discard
      run("def mkFn(a: AnyRef^) = () => a.toString()")
    } andThen {
      storedOutput() // discard
      run(":type mkFn(new Object)")
      assertEquals("() -> String", storedOutput().trim)
    }

  // :type should display read-only captures (.rd)
  @Test def `cc type command shows rd capture` =
    initially {
      run("import language.experimental.captureChecking")
      run("import caps.Mutable")
    } andThen {
      storedOutput()
      run("class Ref(init: Int) extends Mutable { private var current = init; def get: Int = current; update def put(x: Int): Unit = current = x }")
    } andThen {
      storedOutput()
      run("def readRef(r: Ref^{caps.any.rd}): Int = r.get")
    } andThen {
      storedOutput()
      // :type on a method reference shows its eta-expanded function type
      run(":type readRef")
      assertEquals("Ref^{any.rd} -> Int", storedOutput().trim)
    }

  // :type should display classifier-restricted captures (.only[...])
  @Test def `cc type command shows classifier` =
    initially {
      run("import language.experimental.captureChecking")
      run("import caps.{SharedCapability, Classifier}")
    } andThen {
      storedOutput()
      run("trait Control extends SharedCapability, Classifier")
    } andThen {
      storedOutput()
      run("def restricted(f: () ->{caps.any.only[Control]} Unit): Unit = f()")
    } andThen {
      storedOutput()
      run(":type restricted")
      assertEquals("(() ->{any.only[Control]} Unit) -> Unit", storedOutput().trim)
    }

  // :type should display capture sets on values
  @Test def `cc type command shows captures on value` =
    initially {
      run("import language.experimental.captureChecking")
    } andThen {
      storedOutput()
      run("def withCaps(a: AnyRef^, b: AnyRef^): AnyRef^{a, b} = a")
    } andThen {
      storedOutput()
      run(":type withCaps")
      assertEquals("(a: AnyRef^, b: AnyRef^) -> Object^{a, b}", storedOutput().trim)
    }

  // scala/scala3#25465: :doc on a type should give a proper error with CC, not crash
  @Test def `i25465 doc command with CC enabled` =
    initially {
      run("import language.experimental.captureChecking")
    } andThen {
      storedOutput() // discard
      run("trait Foo")
    } andThen {
      storedOutput() // discard
      run(":doc Foo")
      val output = storedOutput().trim
      assertTrue(s"Expected error about term/type mismatch, got: $output",
        output.contains("Not Found Error") || output.contains("Expected a term"))
    }
