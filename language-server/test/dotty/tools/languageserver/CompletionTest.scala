package dotty.tools.languageserver

import org.junit.Assert.{assertTrue, assertFalse}
import org.junit.Test
import org.eclipse.lsp4j.CompletionItemKind._

import dotty.tools.languageserver.util.Code._

class CompletionTest {

  @Test def completion0: Unit = {
    code"class Foo { val xyz: Int = 0; def y: Int = xy$m1 }".withSource
      .completion(m1, Set(("xyz", Field, "Int")))
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
    ).completion(m1, Set(("MyClass", Class, "Object{...}")))
  }

  @Test def ImportCompleteClassNoPrefix: Unit = {
    withSources(
      code"""object Foo { class MyClass }""",
      code"""import Foo.${m1}"""
    ).completion(m1, results => {
      val myClass = ("MyClass", Class, "Object{...}")
      assertTrue(results.contains(("MyClass", Class, "Object{...}")))

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
    ).completion(m1, Set(("MyClass", Class, "Object{...}")))
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
    ).completion(m1, Set(("MyObject", Module, "O.MyObject")))
  }

  @Test def importCompleteWithClassAndCompanion: Unit = {
    withSources(
      code"""package pkg0
             class Foo
             object Foo""",
      code"""package pgk1
             import pkg0.F${m1}"""
    ).completion(m1, Set(("Foo", Class, "Object{...}")))
  }

  @Test def importCompleteIncludePackage: Unit = {
    withSources(
      code"""package foo.bar
             class Fizz""",
      code"""import foo.b${m1}"""
    ).completion(m1, Set(("bar", Module, "{...}")))
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
                         ("myObject", Module, "MyObject.myObject"),
                         ("myClass", Class, "Object{...}"),
                         ("myTrait", Class, "Object{...}")))
  }

  @Test def importJavaClass: Unit = {
    code"""import java.io.FileDesc${m1}""".withSource
      .completion(m1, Set(("FileDescriptor", Class, "Object{...}")))
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
      .completion(m1, Set(("FileDescriptor", Module, "java.io.FileDescriptor")))
  }

  @Test def importRename: Unit = {
    code"""import java.io.{FileDesc${m1} => Foo}""".withSource
      .completion(m1, Set(("FileDescriptor", Class, "Object{...}")))
  }
}
