package dotty.tools.languageserver

import org.junit.Assert.{assertEquals, assertTrue, assertFalse}
import org.junit.Test
import org.eclipse.lsp4j.CompletionItemKind._

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.actions.CodeCompletion

class CompletionTest {

  @Test def completion0: Unit = {
    code"class Foo { val xyz: Int = 0; def y: Int = xy${m1} }".withSource
      .completion(m1, Set(("xyz", Field, "Int")))
  }

  @Test def completionFromScalaPredef: Unit = {
    code"class Foo { def foo: Unit = prin${m1} }".withSource
      .completion(m1, Set(
        ("print", Method, "(x: Any): Unit"),
        ("printf", Method, "(text: String, xs: Any*): Unit"),
        ("println", Method, "(x: Any): Unit")
      ))
  }

  @Test def completionFromNewScalaPredef: Unit = {
    code"class Foo { val foo = summ${m1} }".withSource
      .completion(m1, Set(("summon", Method, "[T](using x: T): x.type")))
  }

  @Test def completionFromScalaPackage: Unit = {
    code"class Foo { val foo: Conv${m1} }".withSource
      .completion(m1, Set(("Conversion", Class, "scala.Conversion")))
  }

  @Test def completionFromScalaPackageObject: Unit = {
    code"class Foo { val foo: BigD${m1} }".withSource
      .completion(m1, Set(("BigDecimal", Field, "type and getter BigDecimal")))
  }

  @Test def completionFromSyntheticPackageObject: Unit = {
    code"class Foo { val foo: IArr${m1} }".withSource
      .completion(m1, Set(("IArray", Field, "type and object IArray")))
  }

  @Test def completionFromJavaDefaults: Unit = {
    code"class Foo { val foo: Runn${m1} }".withSource
      .completion(m1, Set(("Runnable", Class, "trait and object Runnable")))
  }

  @Test def completionWithImplicitConversion: Unit = {
    withSources(
      code"object Foo { implicit class WithBaz(bar: Bar) { def baz = 0 } }",
      code"class Bar",
      code"object Main { import Foo._; val bar: Bar = new Bar; bar.b${m1} }"
    ) .completion(m1, Set(("baz", Method, "=> Int")))
  }

  @Test def importCompleteClassWithPrefix: Unit = {
    withSources(
      code"""object Foo { class MyClass }""",
      code"""import Foo.My${m1}"""
    ).completion(m1, Set(("MyClass", Class, "Foo.MyClass")))
  }

  @Test def importCompleteClassNoPrefix: Unit = {
    withSources(
      code"""object Foo { class MyClass }""",
      code"""import Foo.${m1}"""
    ).completion(m1, completionItems => {
      val results = CodeCompletion.simplifyResults(completionItems)
      val myClass = ("MyClass", Class, "Foo.MyClass")
      assertTrue(results.contains(("MyClass", Class, "Foo.MyClass")))

      // Verify that apart from `MyClass`, we only have the methods that exists on `Foo`
      assertTrue((results - myClass).forall { case (_, kind, _) => kind == Method })

      // Verify that we don't have things coming from an implicit conversion, such as ensuring
      assertFalse(results.exists { case (name, _, _) => name == "ensuring" })
    })
  }

  @Test def importCompleteFromPackage: Unit = {
    withSources(
      code"""package a
             class MyClass""",
      code"""package b
             import a.My${m1}"""
    ).completion(m1, Set(("MyClass", Class, "a.MyClass")))
  }

  @Test def importCompleteFromClass: Unit = {
    withSources(
      code"""class Foo { val x: Int = 0 }""",
      code"""import Foo.${m1}"""
    ).completion(m1, Set())
  }

  @Test def importCompleteIncludesSynthetic: Unit = {
    code"""case class MyCaseClass(foobar: Int)
           object O {
             val x = MyCaseClass(0)
             import x.c${m1}
           }""".withSource
      .completion(
        m1,
        Set(("clone", Method, "(): Object"),
            ("copy", Method, "(foobar: Int): MyCaseClass"),
            ("canEqual", Method, "(that: Any): Boolean")))
  }

  @Test def importCompleteIncludeModule: Unit = {
    withSources(
      code"""object O { object MyObject }""",
      code"""import O.My${m1}"""
    ).completion(m1, Set(("MyObject", Module, "O.MyObject$")))
  }

  @Test def importCompleteWithClassAndCompanion: Unit = {
    withSources(
      code"""package pkg0
             class Foo
             object Foo""",
      code"""package pgk1
             import pkg0.F${m1}"""
    ).completion(m1, Set(("Foo", Class, "class and object Foo")))
  }

  @Test def importCompleteIncludePackage: Unit = {
    withSources(
      code"""package foo.bar
             class Fizz""",
      code"""import foo.b${m1}"""
    ).completion(m1, Set(("bar", Module, "foo.bar")))
  }

  @Test def importCompleteIncludeMembers: Unit = {
    withSources(
      code"""object MyObject {
               val myVal = 0
               def myDef = 0
               var myVar = 0
               object myObject
               class myClass
               trait myTrait
             }""",
      code"""import MyObject.my${m1}"""
    ).completion(m1, Set(("myVal", Field, "Int"),
                         ("myDef", Method, "=> Int"),
                         ("myVar", Variable, "Int"),
                         ("myObject", Module, "MyObject.myObject$"),
                         ("myClass", Class, "MyObject.myClass"),
                         ("myTrait", Class, "MyObject.myTrait")))
  }

  @Test def importJavaClass: Unit = {
    code"""import java.io.FileDesc${m1}""".withSource
      .completion(m1, Set(("FileDescriptor", Class, "class and object FileDescriptor")))
  }

  @Test def importJavaStaticMethod: Unit = {
    code"""import java.lang.System.lineSep${m1}""".withSource
      .completion(m1, Set(("lineSeparator", Method, "(): String")))
  }

  @Test def importJavaStaticField: Unit = {
    code"""import java.lang.System.ou${m1}""".withSource
      .completion(m1, Set(("out", Field, "java.io.PrintStream")))
  }

  @Test def completeJavaModuleClass: Unit = {
    code"""object O {
             val out = java.io.FileDesc${m1}
           }""".withSource
      .completion(m1, Set(("FileDescriptor", Module, "java.io.FileDescriptor$")))
  }

  @Test def importRename: Unit = {
    code"""import java.io.{FileDesc${m1} => Foo}""".withSource
      .completion(m1, Set(("FileDescriptor", Class, "class and object FileDescriptor")))
  }

  @Test def markDeprecatedSymbols: Unit = {
    code"""object Foo {
             @deprecated
             val bar = 0
           }
           import Foo.ba${m1}""".withSource
      .completion(m1, results => {
        assertEquals(1, results.size)
        val result = results.head
        assertEquals("bar", result.getLabel)
        assertTrue("bar was not deprecated", result.getDeprecated)
      })
  }

  @Test def i4397: Unit = {
    code"""class Foo {
          |  .${m1}
          |}""".withSource
      .completion(m1, Set())
  }

  @Test def completeNoPrefix: Unit = {
    code"""class Foo { def foo = 0 }
          |object Bar {
          |  val foo = new Foo
          |  foo.${m1}
          |}""".withSource
      .completion(m1, results => assertTrue(results.nonEmpty))
  }

  @Test def completeErrorKnowsKind: Unit = {
    code"""object Bar {
          |  class Zig
          |  val Zag: Int = 0
          |  val b = 3 + Bar.${m1}
          |}""".withSource
      .completion(m1, completionItems => {
        val results = CodeCompletion.simplifyResults(completionItems)
        assertTrue(results.contains(("Zag", Field, "Int")))
        assertFalse(results.exists((label, _, _) => label == "Zig"))
      })
  }

  @Test def typeCompletionShowsTerm: Unit = {
    code"""class Bar
          |object Foo {
          |  val bar = new Bar
          |  def baz = new Bar
          |  object bat
          |  val bizz: ba${m1}
          |}""".withSource
      .completion(m1, Set(("bar", Field, "Bar"), ("bat", Module, "Foo.bat$")))
  }

  @Test def completionOnRenamedImport: Unit = {
    code"""import java.io.{FileDescriptor => AwesomeStuff}
           trait Foo { val x: Awesom$m1 }""".withSource
      .completion(m1, Set(("AwesomeStuff", Class, "class and object FileDescriptor")))
  }

  @Test def completionOnRenamedImport2: Unit = {
    code"""import java.util.{HashMap => MyImportedSymbol}
           trait Foo {
             import java.io.{FileDescriptor => MyImportedSymbol}
             val x: MyImp$m1
           }""".withSource
      .completion(m1, Set(("MyImportedSymbol", Class, "class and object FileDescriptor")))
  }

  @Test def completionRenamedAndOriginalNames: Unit = {
    code"""import java.util.HashMap
          |trait Foo {
          |  import java.util.{HashMap => HashMap2}
          |  val x: Hash$m1
          |}""".withSource
      .completion(m1, Set(("HashMap", Class, "class and object HashMap"),
                          ("HashMap2", Class, "class and object HashMap")))
  }

  @Test def completionRenamedThrice: Unit = {
    code"""import java.util.{HashMap => MyHashMap}
          |import java.util.{HashMap => MyHashMap2}
          |trait Foo {
          |  import java.util.{HashMap => MyHashMap3}
          |  val x: MyHash$m1
          |}""".withSource
      .completion(m1, Set(("MyHashMap", Class, "class and object HashMap"),
                          ("MyHashMap2", Class, "class and object HashMap"),
                          ("MyHashMap3", Class, "class and object HashMap")))
  }

  @Test def completionClassAndMethod: Unit = {
    code"""object Foo {
          |  class bar
          |  def bar = 0
          |}
          |import Foo.b$m1""".withSource
      .completion(m1, Set(("bar", Class, "class and method bar")))
  }

  @Test def completionTypeAndLazyValue: Unit = {
    code"""object Foo {
          |  type bar = Int
          |  lazy val bar = 3
          |}
          |import Foo.b$m1""".withSource
      .completion(m1, Set(("bar", Field, "type and lazy value bar")))
  }

  @Test def completeExtensionMethodWithoutParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodWithParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx(i: Int) = i
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "(i: Int): Int")))
  }

  @Test def completeExtensionMethodWithTypeParameter: Unit = {
    code"""object Foo
          |extension [A](foo: Foo.type) def xxxx: Int = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "[A] => Int")))
  }

  @Test def completeExtensionMethodWithParameterAndTypeParameter: Unit = {
    code"""object Foo
          |extension [A](foo: Foo.type) def xxxx(a: A) = a
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "[A](a: A): A")))
  }

  @Test def completeExtensionMethodFromExtenionWithAUsingSection: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar = new Bar {}
          |given Baz = new Baz {}
          |extension (foo: Foo.type)(using Bar, Baz) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "(using x$1: Bar, x$2: Baz): Int")))
  }

  @Test def completeExtensionMethodFromExtenionWithMultipleUsingSections: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar = new Bar {}
          |given Baz = new Baz {}
          |extension (foo: Foo.type)(using Bar)(using Baz) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "(using x$1: Bar)(using x$2: Baz): Int")))
  }

  @Test def completeInheritedExtensionMethod: Unit = {
    code"""object Foo
          |trait FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main extends FooOps { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeRenamedExtensionMethod: Unit = {
    code"""object Foo
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |import FooOps.{xxxx => yyyy}
          |object Main { Foo.yy${m1} }""".withSource
      .completion(m1, Set(("yyyy", Method, "=> Int")))
  }

  @Test def completeExtensionMethodFromGivenInstanceDefinedInScope: Unit = {
    code"""object Foo
          |trait FooOps
          |given FooOps with {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodFromImportedGivenInstance: Unit = {
    code"""object Foo
          |trait FooOps
          |object Bar {
          |  given FooOps with {
          |    extension (foo: Foo.type) def xxxx = 1
          |  }
          |}
          |import Bar.given
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodFromImplicitScope: Unit = {
    code"""case class Foo(i: Int)
          |object Foo {
          |  extension (foo: Foo) def xxxx = foo.i
          |}
          |object Main { Foo(123).xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodFromGivenInImplicitScope: Unit = {
    code"""trait Bar
          |case class Foo(i: Int)
          |object Foo {
          |  given Bar with {
          |    extension (foo: Foo) def xxxx = foo.i
          |  }
          |}
          |object Main { Foo(123).xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodOnResultOfImplicitConversion: Unit = {
    code"""import scala.language.implicitConversions
          |case class Foo(i: Int)
          |extension (foo: Foo) def xxxx = foo.i
          |given Conversion[Int, Foo] = Foo(_)
          |object Main { 123.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def dontCompleteExtensionMethodWithMismatchedName: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object Main { Foo.yy${m1} }""".withSource
      .completion(m1, Set())
  }

  @Test def preferNormalMethodToExtensionMethod: Unit = {
    code"""object Foo {
          |  def xxxx = "abcd"
          |}
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> String")))
  }

  @Test def preferExtensionMethodFromExplicitScope: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = "abcd"
          |}
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def dontCompleteInapplicableExtensionMethod: Unit = {
    code"""case class Foo[A](a: A)
          |extension (foo: Foo[Int]) def xxxx = foo.a
          |object Main { Foo("abc").xx${m1} }""".withSource
      .completion(m1, Set())
  }
}
