package org.scalajs.testsuite.jsinterop

import org.junit.Assert.*
import org.junit.Test

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobal("interoperabilityTestGlobalValDefConstant")
val interoperabilityTestGlobalValDefConstant: Int = js.native

@js.native
@JSGlobal("interoperabilityTestGlobalValDefVariable")
def interoperabilityTestGlobalValDefVariable: Int = js.native

@js.native
@JSGlobal("interoperabilityTestGlobalValDefGetVariable")
def interoperabilityTestGlobalValDefGetVariable(): Int = js.native

@js.native
@JSGlobal("interoperabilityTestGlobalValDefSetVariable")
def interoperabilityTestGlobalValDefSetVariable(x: Int): Unit = js.native

@js.native
@JSGlobal("interoperabilityTestGlobalValDefFunction")
def interoperabilityTestGlobalValDefFunction(x: Int): Int = js.native

@js.native
@JSImport("querystring", "stringify")
def stringify(obj: js.Dictionary[String], sep: String = "&", eq: String = "="): String = js.native

@js.native
@JSImport("os", "EOL")
val EOL: String = js.native

@js.native
@JSImport("os", "EOL")
def EOLAsDef: String = js.native

class TopLevelNativeJSMembersTestScala3 {
  @Test def should_access_top_level_JS_vars_and_functions_with_top_level_native_vals_and_defs(): Unit = {
    js.eval("""
      var interoperabilityTestGlobalValDefConstant = 654321;
      var interoperabilityTestGlobalValDefVariable = 7357;
      var interoperabilityTestGlobalValDefGetVariable = function() {
        return interoperabilityTestGlobalValDefVariable;
      }
      var interoperabilityTestGlobalValDefSetVariable = function(x) {
        interoperabilityTestGlobalValDefVariable = x;
      }
      var interoperabilityTestGlobalValDefFunction = function(x) {
        return interoperabilityTestGlobalValDefVariable + x;
      };
    """)

    assertEquals(654321, interoperabilityTestGlobalValDefConstant)

    assertEquals(7357, interoperabilityTestGlobalValDefVariable)
    assertEquals(7357, interoperabilityTestGlobalValDefGetVariable())
    assertEquals(7360, interoperabilityTestGlobalValDefFunction(3))

    interoperabilityTestGlobalValDefSetVariable(123)
    assertEquals(123, interoperabilityTestGlobalValDefGetVariable())
    assertEquals(126, interoperabilityTestGlobalValDefFunction(3))
  }

  /*
  Ideally we would like to add those two tests as well.
  However, we do not have infrastructure yet in the Dotty build to test Scala.js
  with module support enabled, so the linker will refuse to link the @JSImports.

  @Test def testImportFunctionInModule(): Unit = {
    val dict = js.Dictionary("foo" -> "bar", "baz" -> "qux")

    assertEquals("foo=bar&baz=qux", stringify(dict))
    assertEquals("foo:bar;baz:qux", stringify(dict, ";", ":"))
  }

  @Test def testImportFieldInModule(): Unit = {
    assertEquals("string", js.typeOf(EOL))
    assertEquals("string", js.typeOf(EOLAsDef))
  }
  */
}
